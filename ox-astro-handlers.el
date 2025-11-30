;;; ox-astro-handlers.el --- Filter and handler functions for ox-astro  -*- lexical-binding: t -*-

;;; Commentary:
;; Filter and handler functions for ox-astro export

;;; Code:

(require 'org)
(require 'org-element)
(require 'ox)
(require 'ox-astro-config)
(require 'cl-lib)
(require 'subr-x)

;; Declare functions from other ox-astro modules
(declare-function org-astro--collect-images-from-tree "ox-astro-image-handlers")
(declare-function org-astro--build-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--build-render-map "ox-astro-image-handlers")
(declare-function org-astro--process-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--persist-wrap-raw-image-lines "ox-astro-image-handlers")
(declare-function org-astro--get-assets-folder "ox-astro-helpers")
(declare-function org-astro--sanitize-filename "ox-astro-helpers")
(declare-function org-astro--path-to-var-name "ox-astro-helpers")
(declare-function org-astro--get-front-matter-data "ox-astro-helpers")
(declare-function org-astro--validate-front-matter "ox-astro-helpers")
(declare-function org-astro--gen-yaml-front-matter "ox-astro-helpers")
(declare-function org-astro--dbg-log "ox-astro-helpers")
(declare-function org-astro--slugify "ox-astro-helpers")
(declare-function org-astro--get-title "ox-astro-helpers")
(declare-function org-astro--hero-image-entry-p "ox-astro-helpers")
(declare-function org-astro--normalize-image-path "ox-astro-helpers")
(declare-function org-astro--resolve-destination-config "ox-astro-helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variable to persist data across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

(defun org-astro--ordered-item-number (item struct)
  "Return 1-based index for ITEM counting only numeric siblings in STRUCT."
  (let ((pos (org-element-property :begin item))
        (count 0))
    (catch 'found
      (cl-loop for entry in struct
               for entry-pos = (nth 0 entry)
               for entry-bullet = (nth 2 entry)
               do (cond
                   ((< entry-pos pos)
                    (when (and entry-bullet
                               (string-match-p "^[0-9]+\\." entry-bullet))
                      (cl-incf count)))
                   ((= entry-pos pos)
                    (throw 'found (1+ count)))))
      (1+ count))))

(defun org-astro-item (item contents info)
  "Transcode ITEM into Markdown, honoring mixed list bullets.
When an ordered list contains literal dash bullets (Org treats them as
ordered siblings), downgrade those entries to nested unordered bullets so
the MDX output preserves the intended hierarchy."
  (let* ((parent (org-element-property :parent item))
         (parent-type (org-element-property :type parent))
         (raw-bullet (org-element-property :bullet item))
         (struct (org-element-property :structure item))
         (unordered-marker (and raw-bullet
                                (string-match-p "^[+*-]" (string-trim raw-bullet))))
         ;; Treat literal dash/plus/star bullets inside ordered lists as unordered.
         (effective-type (if unordered-marker 'unordered parent-type))
         (numeric-marker (org-astro--ordered-item-number item struct))
         (bullet (if (eq effective-type 'ordered)
                     (concat (number-to-string (or numeric-marker 1)) ".")
                   "-"))
         ;; Indent pseudo-nested dash bullets so Markdown treats them as children.
         (indent-prefix (if (and unordered-marker (not (eq parent-type 'unordered)))
                           "    "
                         "")))
    (concat indent-prefix
            bullet
            (make-string (max 1 (- 4 (length bullet))) ? )
            (pcase (org-element-property :checkbox item)
              (`on "[X] ")
              (`trans "[-] ")
              (`off "[ ] "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "**%s:** " (org-export-data tag info))))
            (and contents
                 (let* ((indented (replace-regexp-in-string
                                   "^"
                                   (concat indent-prefix "    ")
                                   contents)))
                   (org-trim indented))))))

(defun org-astro-property-drawer (_property-drawer _contents _info)
  "Transcode a PROPERTY-DRAWER element.
Property drawers are suppressed in Astro MDX output - they contain
org-roam metadata that belongs in frontmatter, not the body."
  "")

(defun org-astro-keyword (keyword _contents _info)
  "Transcode THEME and MODEL keywords into inline markers.
#+THEME: emits a JSX comment marker for remarkThemeSections.
#+MODEL: emits a visible model banner div.
Other keywords defer to the markdown backend."
  (let* ((key (org-element-property :key keyword))
         (value (string-trim (org-element-property :value keyword)))
         ;; Determine if this keyword is in the body (after the first heading)
         (tree (plist-get _info :parse-tree))
         (first-headline-pos (when tree
                               (org-element-map tree 'headline
                                 (lambda (h) (org-element-property :begin h))
                                 nil 'first-match)))
         (pos (org-element-property :begin keyword))
         (body-level (or (null first-headline-pos)
                         (and pos first-headline-pos (>= pos first-headline-pos)))))
    (cond
     ;; #+THEME: → JSX comment marker
     ((string-equal key "THEME")
      (if body-level
          (format "{/* theme: %s */}\n\n" (downcase value))
        ""))
     ;; #+MODEL: → visible banner div
     ((string-equal key "MODEL")
      (if body-level
          (format "<div class=\"model-banner\">%s</div>\n\n" value)
        ""))
     ;; #+IMAGE-CREDIT/CAPTION/PROMPT: → suppress (handled by image rendering)
     ((member key '("IMAGE-CREDIT" "IMAGE_CREDIT"
                    "IMAGE-CAPTION" "IMAGE_CAPTION"
                    "IMAGE-PROMPT" "IMAGE_PROMPT"))
      "")
     (t (org-md-keyword keyword _contents _info)))))

(defun org-astro--collect-image-metadata-filter (tree _backend info)
  "Collect IMAGE-CREDIT/CAPTION/PROMPT keywords associated with images.
Looks for keywords immediately following an image paragraph (skipping blank
paragraphs). Keys are normalized with source context to avoid collisions."
  (let ((credit-map (make-hash-table :test 'equal))
        (caption-map (make-hash-table :test 'equal))
        (source-file (plist-get info :input-file)))
    (org-element-map tree 'paragraph
      (lambda (paragraph)
        ;; Find the first image link in this paragraph
        (let ((img-link (org-element-map paragraph 'link
                          (lambda (link)
                            (let ((path (org-element-property :path link)))
                              (when (and path
                                         (string-match-p "\\.\\(png\\|jpe?g\\|webp\\|gif\\)$" path))
                                link)))
                          nil 'first-match)))
          (when img-link
            (let* ((img-path (org-element-property :path img-link))
                   (parent (org-element-parent paragraph))
                   (siblings (and parent (org-element-contents parent)))
                   (idx (and siblings (cl-position paragraph siblings :test #'eq)))
                   (norm-key (and img-path (org-astro--normalize-image-key img-path source-file))))
              (when (and idx norm-key)
                ;; Walk forward through subsequent siblings collecting IMAGE-* keywords
                (cl-loop for j from (1+ idx) below (length siblings)
                         for sib = (nth j siblings)
                         for type = (org-element-type sib)
                         do (cond
                             ;; Skip over blank paragraphs
                             ((and (eq type 'paragraph)
                                   (string-blank-p (org-element-interpret-data sib)))
                              nil)
                             ;; Collect IMAGE-* keywords
                             ((and (eq type 'keyword)
                                   (member (org-element-property :key sib)
                                           '("IMAGE-CREDIT" "IMAGE_CREDIT"
                                             "IMAGE-CAPTION" "IMAGE_CAPTION"
                                             "IMAGE-PROMPT" "IMAGE_PROMPT")))
                              (let* ((key (org-element-property :key sib))
                                     (value (string-trim (or (org-element-property :value sib) ""))))
                                (unless (string-empty-p value)
                                  (cond
                                   ((member key '("IMAGE-CREDIT" "IMAGE_CREDIT"))
                                    (puthash norm-key value credit-map))
                                   (t
                                    (puthash norm-key value caption-map))))))
                             ;; Stop at the first non-keyword, non-blank element
                             (t (cl-return))))))))))
    (plist-put info :astro-image-credits credit-map)
    (plist-put info :astro-image-captions caption-map))
  tree)

(defun org-astro--ensure-inline-theme-markers (tree _backend _info)
  "Ensure theme markers precede top-level Claude/ChatGPT headings.
If a level-1 heading title is exactly \"Claude\" or \"ChatGPT\", insert or move
the corresponding THEME keyword immediately before it in the parse tree. This
keeps inline theme sections aligned with remarkThemeSections without manual
marker placement in Org."
  (let ((children (org-element-contents tree))
        (result '()))
    (cl-labels
        ((theme-keyword-p
          (node theme)
          (and (eq (org-element-type node) 'keyword)
               (string-equal (org-element-property :key node) "THEME")
               (string-equal (downcase (org-element-property :value node)) theme)))
         (make-theme-node
          (theme)
          (org-element-create 'keyword
                              (list :key "THEME"
                                    :value theme
                                    :raw-value theme
                                    :post-blank 1)))))
      (while children
        (let* ((node (car children))
               (next (cadr children)))
          (if (and (eq (org-element-type node) 'headline)
                   (= 1 (org-element-property :level node))
                   (member (downcase (org-element-property :raw-value node))
                           '("claude" "chatgpt")))
              (let* ((theme (downcase (org-element-property :raw-value node))))
                ;; Ensure correct theme keyword immediately before headline.
                (cond
                 ;; Already have the correct theme keyword immediately before.
                 ((and result (theme-keyword-p (car (last result)) theme))
                  nil)
                 ;; If the next node is the correct theme keyword, move it before.
                 ((theme-keyword-p next theme)
                  (setq children (cdr children)) ; drop the keyword from its current spot
                  (push (make-theme-node theme) result))
                 (t
                  (push (make-theme-node theme) result)))
                ;; Always push the headline itself
                (push node result))
            ;; Non-target nodes pass through unchanged
            (push node result)))
        (setq children (cdr children)))
      (org-element-set-contents tree (nreverse result)))
  tree)

(defun org-astro-auto-wrap-image-paths-filter (tree _backend info)
  "Pre-processing filter that automatically wraps raw image paths in [[ ]] brackets.
This runs FIRST, before all other processing, to simulate manual bracket addition."
  ;; Just wrap raw paths - keep it simple
  (let ((src-file (or (plist-get info :input-file)
                      (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
    (when src-file
      ;; Step 1: Wrap raw paths in brackets and save
      (let ((count (org-astro--persist-wrap-raw-image-lines src-file)))
        (when (> count 0)
          (message "Auto-wrapped %d raw image paths in source file" count)))))

  ;; Return tree unchanged
  tree)



(defun org-astro-prepare-images-filter (tree _backend info)
  "Ensure image import metadata is present on INFO for later phases."
  (org-astro--dbg-log info "=== Starting prepare-images-filter ===")
  ;; Reset transient state so we always repopulate from the most recent context.
  (setq org-astro--current-body-images-imports nil)
  (plist-put info :astro-body-images-imports nil)
  (plist-put info :astro-uses-linkpeek nil)

  (let* ((context (plist-get info :astro-export-context))
         (_ (when context
              (let ((manifest (plist-get context :manifest)))
                (when manifest
                  (setq info (cl-putf info :astro-image-manifest manifest))))))
         (processed (plist-get context :processed)))
    (unless processed
      (let* ((posts-folder-raw (or (plist-get info :destination-folder)
                                   (plist-get info :astro-posts-folder)))
             (destination-info (org-astro--resolve-destination-config posts-folder-raw))
             (posts-folder (plist-get destination-info :path))
             (posts-folder-key (or (plist-get destination-info :raw)
                                   posts-folder-raw))
             (manifest (or (plist-get info :astro-image-manifest)
                           (org-astro--build-image-manifest tree info)))
             (title (org-astro--get-title tree info))
             (slug (or (plist-get info :slug)
                       (let* ((title-kw (org-element-map tree 'keyword
                                          (lambda (k)
                                            (when (string-equal "TITLE" (org-element-property :key k)) k))
                                          nil 'first-match))
                              (title-from-headline (not title-kw)))
                         (when title-from-headline
                           (org-astro--slugify title)))))
             (sub-dir (if slug (concat "posts/" slug "/") "posts/"))
             (result (when posts-folder
                       (org-astro--process-image-manifest manifest posts-folder sub-dir)))
             (processed-result (plist-get result :entries))
             (refreshed-context (list :manifest manifest
                                      :processed processed-result
                                      :posts-folder posts-folder
                                      :sub-dir sub-dir)))
        (when posts-folder-key
          (plist-put info :destination-folder posts-folder-key))
        (when manifest
          (setq info (cl-putf info :astro-image-manifest manifest)))
        (setq info (cl-putf info :astro-export-context refreshed-context))
        (setq processed processed-result)))

    ;; Persist the processed list (from either preflight or fallback work).
    (when processed
      (plist-put info :astro-body-images-imports processed)
      (setq org-astro--current-body-images-imports processed))
    ;; Build render metadata for downstream consumers.
    (let* ((render-data (and processed (org-astro--build-render-map processed (plist-get info :astro-hero-image))))
           (render-map (and render-data (plist-get render-data :map)))
           (render-imports (and render-data (plist-get render-data :imports))))
      (plist-put info :astro-render-map render-map)
      (plist-put info :astro-render-imports render-imports)))
  tree)



(defun org-astro-body-filter (body _backend info)
  "Add front-matter, source comment, and imports to BODY."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--validate-front-matter
                             (org-astro--get-front-matter-data tree info)))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         (processed-images (plist-get info :astro-body-images-imports))
         (hero-path (or (plist-get info :astro-hero-image)
                        (let* ((image-entry (assoc 'image front-matter-data))
                               (fallback (and image-entry (cdr image-entry)))
                               (normalized (and fallback (org-astro--normalize-image-path fallback))))
                          (when normalized
                            (setf (plist-get info :astro-hero-image) normalized))
                          normalized)))
         (render-refresh (and processed-images hero-path
                               (org-astro--build-render-map processed-images hero-path)))
         (_ (when render-refresh
              (plist-put info :astro-render-map (plist-get render-refresh :map))
              (plist-put info :astro-render-imports (plist-get render-refresh :imports))))
         (hero-record
          (when render-refresh
            (let ((map (plist-get render-refresh :map))
                  (found nil))
              (when (hash-table-p map)
                (maphash
                 (lambda (_ record)
                   (when (and (listp record)
                              (or (plist-get record :hero)
                                  (and hero-path
                                       (let ((entry (plist-get record :entry)))
                                         (and entry
                                              (org-astro--hero-image-entry-p entry hero-path)))))
                              (not found))
                     (setq found record)))
                 map))
              found)))
         (_ (when hero-record
              (let* ((hero-var (plist-get hero-record :var-name))
                     (hero-pattern (and hero-var
                                        (format "<Image\\(?:.\\|\\n\\)*?src={%s}\\(?:.\\|\\n\\)*?\\(?:/>\\|>\\(?:.\\|\\n\\)*?</Image>\\)"
                                                (regexp-quote hero-var)))))
                (when (and hero-pattern (string-match hero-pattern body))
                  (let ((case-fold-search nil))
                    (setq body (replace-match "" t t body)))
                 (let* ((used-vars (plist-get info :astro-render-used-vars))
                        (updated (cl-remove hero-var used-vars :test #'equal)))
                   (setf (plist-get info :astro-render-used-vars) updated))))))
         ;; Strip orphan property drawers and ID lines that Org leaves as text.
         (_ (setq body (replace-regexp-in-string
                        "^:PROPERTIES:\n\\(?:[^\n]*\n\\)*?:END:\n?"
                        "" body)))
         (_ (setq body (replace-regexp-in-string
                        "^:ID:\\s-+.*\n?"
                        "" body t)))
         ;; Copy frontmatter to clipboard (opt-in)
         (_ (when (and (boundp 'org-astro-copy-to-clipboard) org-astro-copy-to-clipboard)
              (let ((pbcopy (executable-find "pbcopy")))
                (when (and pbcopy front-matter-string)
                  (condition-case _
                      (with-temp-buffer
                        (insert front-matter-string)
                        (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                    (error nil))))))
         ;; Add an HTML comment noting the source .org file path, placed
         ;; after the frontmatter (frontmatter should remain at top-of-file).
         (source-path (or (plist-get info :input-file)
                          (and (buffer-file-name) (expand-file-name (buffer-file-name)))))
         (source-comment (when source-path
                           (format "{/* Source org: %s */}\n" source-path)))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (render-imports (plist-get info :astro-render-imports))
         (used-image-vars (plist-get info :astro-render-used-vars))
         (render-imports
          (when render-imports
            (let ((filtered render-imports))
              (when (listp filtered)
                (let ((keep-vars used-image-vars))
                  (when keep-vars
                    (setq filtered
                          (cl-remove-if
                           (lambda (line)
                             (and (string-match "^import \\([^ {]+\\) from '" line)
                                  (let ((var (match-string 1 line)))
                                    (and (not (string-prefix-p "{" var))
                                         (not (member var keep-vars))))))
                           filtered)))
                  (unless keep-vars
                    (setq filtered
                          (cl-remove-if
                           (lambda (line)
                             (string-match "^import[[:space:]]*{[[:space:]]*Image[[:space:]]*} from 'astro:assets'" line))
                           filtered)))))
              (when (and (string-match-p "<Image[[:space:]]" body)
                         (not (cl-some (lambda (line)
                                         (string-match "^import[[:space:]]*{[[:space:]]*Image[[:space:]]*} from 'astro:assets'" line))
                                       filtered)))
                (setq filtered (cl-adjoin "import { Image } from 'astro:assets';" filtered :test #'equal)))
              (setq filtered (cl-remove-if #'string-blank-p filtered))
              (setf (plist-get info :astro-render-imports) filtered)
              filtered)))
         (render-imports-string (when render-imports
                                  (mapconcat #'identity render-imports "\n")))
         ;; 2. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 3. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (plist-get info :astro-uses-linkpeek)
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 4. ImageGallery component import (if GALLERY blocks are used)
         (has-gallery-blocks (org-element-map tree 'special-block
                               (lambda (block)
                                 (string-equal (org-element-property :type block) "GALLERY"))
                               nil t))
        (image-gallery-import (when has-gallery-blocks
                                 "import ImageGallery from '@jaydixit/astro-utils/components/ImageGallery.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list render-imports-string linkpeek-import image-gallery-import manual-imports))
                                 "\n")))
    (concat front-matter-string
            (or source-comment "")
            (if (and all-imports (not (string-blank-p all-imports)))
                (concat all-imports "\n\n")
                "")
            body)))

(defun org-astro-final-output-filter (output _backend info)
  "Final filter for Astro export.
- Replaces HTML entities with literal characters.
- Converts indented example blocks to Markdown blockquotes.
- Converts markdown image syntax with absolute paths to Image components."
  (let* ((s output)
         (entity-map '(("&#x2013;" . "–")
                       ("&rsquo;" . "'")
                       ("&lsquo;" . "'")
                       ("&rdquo;" . "\"")
                       ("&ldquo;" . "\""))))
    ;; Replace HTML entities
    (dolist (pair entity-map)
      (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
    ;; Normalize self-closing tags so MDX doesn't expect separate closing tags.
    (let ((case-fold-search t))
      (setq s (replace-regexp-in-string "<br\\s*/?>" "<br />" s)))
    ;; Strip org-roam template boilerplate (Links/Source lines)
    ;; Matches: "    -   **Links:**" or "-  **Source:**" (colon inside or outside bold)
    (setq s (replace-regexp-in-string
             "^[ \t]*-[ \t]+\\*\\*\\(Links\\|Source\\):?\\*\\*[ \t]*:?[ \t]*\n?"
             "" s))
    ;; Also strip standalone "Links:" or "Source:" lines
    (setq s (replace-regexp-in-string
             "^[ \t]*\\(Links\\|Source\\):[ \t]*\n?"
             "" s t))
    ;; Drop orphan property drawers and ID lines that aren't parsed as drawers
    (setq s (replace-regexp-in-string
             "^:PROPERTIES:\n\\(?:[^\n]*\n\\)*?:END:\n?"
             "" s))
    (setq s (replace-regexp-in-string
             "^:ID:\\s-+.*\n?"
             "" s t))
    ;; Convert markdown image syntax with absolute paths to Image components
    (let ((pattern "!\\[\\([^]]*\\)\\](\\([~/][^)]+\\.\\(?:png\\|jpe?g\\|webp\\)\\))"))
      (setq s (replace-regexp-in-string
               pattern
               (lambda (match)
                 (let* ((alt (match-string 1 match))
                        (path (match-string 2 match))
                        (record (org-astro--lookup-render-record path info)))
                   (if record
                       (let ((var-name (plist-get record :var-name)))
                         (if (and var-name (not (string-blank-p alt)))
                             (org-astro--image-component-for-record record info alt path)
                           (plist-get record :jsx)))
                     match)))
               s t t)))
    ;; Indented blocks to blockquotes (but skip JSX components, lists, and code fences)
    (let ((lines (split-string s "\n"))
          (in-jsx-component nil)
          (in-front-matter nil)
          (in-code-fence nil)
          (current-fence nil)
          (in-list-block nil))
      (setq lines
            (mapcar
             (lambda (line)
               (let* ((trimmed (string-trim-left line))
                      (starts-list (or (string-prefix-p "- " trimmed)
                                       (string-prefix-p "* " trimmed)
                                       (string-prefix-p "+ " trimmed)
                                       (string-match-p "^[0-9]+\\.\\s-" trimmed))))
                 (when starts-list
                   (setq in-list-block t))
                 (when (string-match-p "^\\s-*$" line)
                   (setq in-list-block nil))
                 ;; Track fenced code blocks (``` or ~~~) so we don't reflow them
                 (when (string-match "^\\s-*\\(```+\\|~~~+\\)" line)
                   (let ((matched (match-string 1 line)))
                     (if (and in-code-fence (string= matched current-fence))
                         (setq in-code-fence nil
                               current-fence nil)
                       (setq in-code-fence t
                             current-fence matched))))
                 (cond
                  ;; Front matter delimiter toggles
                  ((string= line "---")
                   (setq in-front-matter (not in-front-matter))
                   line)
                  ;; Opening JSX component tag (like <ImageGallery)
                  ((string-match-p "^\\s-*<[A-Z]" line)
                   (setq in-jsx-component t)
                   line)
                  ;; Self-closing tag end (like />)
                  ((and in-jsx-component (string-match-p "/>\\s-*$" line))
                   (setq in-jsx-component nil)
                   line)
                  ;; JSX component closing tag (like >)
                  ((and in-jsx-component (string-match-p "^\\s-*>\\s-*$" line))
                   (setq in-jsx-component nil)
                   line)
                  ;; Don't convert indented lines inside JSX components or front matter
                  ((and (or in-jsx-component in-front-matter)
                        (string-prefix-p "    " line))
                   line)
                  ;; Skip indentation conversion for fenced code or list continuations
                  ((and (string-prefix-p "    " line)
                        (or in-code-fence in-list-block))
                   line)
                  ;; Convert regular indented lines to blockquotes
                  ((string-prefix-p "    " line)
                   (concat "> " (substring line 4)))
                  (t line))))
             lines))
      (setq s (mapconcat #'identity lines "\n")))
    ;; Final pass to ensure any <br> variants are self-closing.
    (let ((case-fold-search t))
      (setq s (replace-regexp-in-string "<br\\s*/?>" "<br />" s)))
    s))

(provide 'ox-astro-handlers)
