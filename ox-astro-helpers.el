;;; ox-astro-helpers.el --- Helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:

(require 'subr-x) ; for string-trim, string-trim-right

;; Declare global variable for data persistence across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

;; Debug helpers
(defun org-astro--dbg-log (info fmt &rest args)
  "Append a formatted debug message to INFO and write to debug.el when enabled."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((msg (apply #'format fmt args))
           (existing (plist-get info :astro-debug-log))
           (line (format "%s\n" (concat "[ox-astro][img] " msg)))
           (debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/debug.el"))
           ;; Check if this is the first log entry
           (first-entry (null existing)))
      ;; Store in info for potential MDX comments or later inspection
      (plist-put info :astro-debug-log (cons msg existing))
      ;; Emit to Messages
      (message "[ox-astro][img] %s" msg)
      ;; Write header if first entry
      (when first-entry
        (let* ((source-file (or (plist-get info :input-file)
                                (and (buffer-file-name) (expand-file-name (buffer-file-name)))
                                "unknown"))
               ;; Try to get actual output file from info if available, otherwise estimate
               (actual-output-file (plist-get info :astro-actual-output-file))
               (posts-folder (or (plist-get info :destination-folder)
                                 (plist-get info :astro-posts-folder)))
               ;; Determine output file path (will be updated later with actual path)
               (output-file (or actual-output-file
                                (when posts-folder
                                  (let* ((title (org-astro--get-title (plist-get info :parse-tree) info))
                                         (slug (when title (org-astro--slugify title)))
                                         (filename (if slug (concat slug ".mdx") "exported-file.mdx"))
                                         (resolved-folder (cdr (assoc posts-folder org-astro-known-posts-folders))))
                                    (when resolved-folder
                                      (expand-file-name filename resolved-folder))))))
               (clipboard-text (format "org-mode source file: %s\n\n.mdx output: %s\n\nAnd please go ahead and access and review the debug file:\n%s\n"
                                       source-file
                                       (or output-file "output file path not yet determined")
                                       debug-file))
               (file-header (format "org-mode source file: %s\n\n.mdx output: %s\n\n"
                                    source-file
                                    (or output-file "output file path not yet determined"))))
          ;; Store header info for potential updates
          (plist-put info :astro-debug-header-info (list :source source-file :output output-file :debug debug-file))
          ;; Write header to debug file
          (condition-case _
              (with-temp-buffer
                (insert file-header)
                (write-region (point-min) (point-max) debug-file nil 'silent))
            (error nil))
          ;; Copy clipboard text to clipboard via pbcopy
          (let ((pbcopy (executable-find "pbcopy")))
            (when pbcopy
              (condition-case _
                  (with-temp-buffer
                    (insert clipboard-text)
                    (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                (error nil))))))
      ;; Append the actual log line
      (condition-case _
          (with-temp-buffer
            (insert line)
            (write-region (point-min) (point-max) debug-file t 'silent))
        (error nil)))))

(defun org-astro--dbg-update-output-file (info actual-output-file)
  "Update the debug file header with the actual output file path."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images actual-output-file)
    (let* ((debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/debug.el"))
           (header-info (plist-get info :astro-debug-header-info)))
      (when header-info
        (let* ((source-file (plist-get header-info :source))
               (clipboard-text (format "org-mode source file: %s\n\n.mdx output: %s\n\nAnd please go ahead and access and review the debug file:\n%s\n"
                                       source-file actual-output-file debug-file))
               (file-header (format "org-mode source file: %s\n\n.mdx output: %s\n\n"
                                    source-file actual-output-file)))
          ;; Update the debug file header
          (condition-case _
              (when (file-exists-p debug-file)
                (with-temp-buffer
                  (insert-file-contents debug-file)
                  (goto-char (point-min))
                  ;; Replace the first few lines (the header) with updated header
                  (when (re-search-forward "^[[ox-astro]]" nil t)
                    (beginning-of-line)
                    (delete-region (point-min) (point))
                    (goto-char (point-min))
                    (insert file-header))
                  (write-region (point-min) (point-max) debug-file nil 'silent)))
            (error nil))
          ;; Update clipboard
          (let ((pbcopy (executable-find "pbcopy")))
            (when pbcopy
              (condition-case _
                  (with-temp-buffer
                    (insert clipboard-text)
                    (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                (error nil)))))))))

(defun org-astro--dbg-mdx-comments (info)
  "Return MDX comments string for any collected debug messages in INFO."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let ((log (plist-get info :astro-debug-log)))
      (when log
        (concat (mapconcat (lambda (s) (format "{/* %s */}" s)) (nreverse log) "\n")
                "\n\n")))))

;; Insert or replace a #+KEY: VALUE line in the top keyword block.
(defun org-astro--upsert-keyword (key value)
  "Upsert #+KEY: VALUE near the top of the buffer or narrowed region.
If KEY exists before the first headline, replace its line.
Otherwise insert after the existing keyword/comment block.
Respects narrowing - works within the current narrowed region."
  (let* ((ukey (upcase (format "%s" key)))
         (re (format "^#\\+%s:\\s-*\\(.*\\)$" (regexp-quote ukey))))
    (save-excursion
      (goto-char (point-min))
      (let ((limit (save-excursion
                     (or (re-search-forward "^\\*" nil t) (point-max)))))
        (if (re-search-forward re limit t)
            ;; Replace existing line
            (progn
              (beginning-of-line)
              (kill-line)
              (insert (format "#+%s: %s" ukey (or value ""))))
            ;; Insert at end of keyword/comment/properties block
            (goto-char (point-min))
            ;; Check if we're in a narrowed subtree (starts with heading)
            (if (looking-at-p "^\\*+ ")
                ;; In narrowed subtree: place keywords after the heading and any properties
                (progn
                  (forward-line 1)  ; Skip the heading line
                  ;; Skip over any properties block
                  (when (looking-at-p "^:PROPERTIES:")
                    (forward-line 1)
                    (while (and (< (point) limit)
                                (not (looking-at-p "^:END:")))
                      (forward-line 1))
                    (when (looking-at-p "^:END:")
                      (forward-line 1)))
                  ;; Skip existing keywords and blank lines
                  (while (and (< (point) limit)
                              (or (looking-at-p "^#\\+")
                                  (looking-at-p "^#\\s-")
                                  (looking-at-p "^\\s-*$")))
                    (forward-line 1)))
                ;; In full file: prefer placing after org-roam preamble (- Links :: / - Source ::)
                (progn
                  ;; First, skip over any org-roam properties block
                  (when (looking-at-p "^:PROPERTIES:")
                    (forward-line 1)
                    (while (and (< (point) limit)
                                (not (looking-at-p "^:END:")))
                      (forward-line 1))
                    (when (looking-at-p "^:END:")
                      (forward-line 1)))
                  ;; If there is a roam preamble (- Links :: or - Source ::) before first headline,
                  ;; insert after the last such line; otherwise fall back to after existing keywords/comments.
                  (let ((roam-anchor (save-excursion
                                       (let ((last-pos nil))
                                         (save-restriction
                                           (narrow-to-region (point-min) limit)
                                           (goto-char (point-min))
                                           (while (re-search-forward "^-[ \t]+\(Links\|Source\) ::[ \t]*$" nil t)
                                             (setq last-pos (line-end-position))))
                                         (when last-pos
                                           (goto-char last-pos)
                                           (forward-line 1)
                                           (point))))))
                    (if roam-anchor
                        (goto-char roam-anchor)
                        ;; Fall back: skip over keywords, comments, and blank lines
                        (while (and (< (point) limit)
                                    (or (looking-at-p "^#\\+")
                                        (looking-at-p "^#\\s-")
                                        (looking-at-p "^\\s-*$")))
                          (forward-line 1))))))
            ;; Keep one blank line before insert unless at BOF or already blank
            (unless (or (bobp) (looking-at-p "^\\s-*$"))
              (insert "\n"))
            (insert (format "#+%s: %s\n" ukey (or value ""))))))))

;; Back-compat alias for existing calls
(defun org-astro--insert-keyword-at-end-of-block (key value)
  "Alias for `org-astro--upsert-keyword'."
  (org-astro--upsert-keyword key value))

(defun org-astro--upsert-keyword-after-roam (key value)
  "Insert #+KEY: VALUE after org-roam preamble if present, else use normal placement."
  (save-excursion
    (goto-char (point-min))
    (let* ((limit (save-excursion (or (re-search-forward "^\\*" nil t) (point-max))))
           (anchor (save-excursion
                     (save-restriction
                       (narrow-to-region (point-min) limit)
                       (goto-char (point-min))
                       (let ((last-pos nil))
                         (while (re-search-forward "^-[ \t]+\\(Links\\|Source\\) ::[ \t]*$" nil t)
                           (setq last-pos (line-end-position)))
                         (when last-pos (goto-char last-pos) (forward-line 1) (point)))))))
      (if anchor
          (progn (goto-char anchor)
                 (unless (or (bobp) (looking-at-p "^\\s-*$")) (insert "\n"))
                 (insert (format "#+%s: %s\n" (upcase key) value)))
          (org-astro--upsert-keyword key value)))))

(defun org-astro--safe-export (data info)
  "Like `org-export-data' but never throws. Falls back to readable plain text."
  (condition-case _
      (org-trim (org-export-data data info))
    (error
     (let* ((s (org-no-properties (org-element-interpret-data data))))
       ;; [[url][desc]] -> desc, [[url]] -> url
       (setq s (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" s))
       (setq s (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\]" "\\1" s))
       ;; Drop common inline markup and collapse whitespace/newlines
       (setq s (replace-regexp-in-string "[*_~/=]" "" s))
       (string-trim (replace-regexp-in-string "\n+" " " s))))))

(defun org-astro--slugify (s)
  "Convert string S to a slug."
  (when (stringp s)
    (let ((s (downcase s)))
      (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil))))

;; Detect whether TEXT contains Markdown link syntax that should be preserved
;; as-is. We check for inline links [text](url) and reference-style links
;; [text][ref]. This is intentionally conservative to avoid false positives.
(defun org-astro--contains-markdown-link-p (text)
  "Return non-nil if TEXT contains Markdown link syntax.
Matches inline links like [label](https://example.com) and
reference-style links like [label][ref]."
  (when (and text (stringp text))
    (or (string-match-p "\\[[^]]+\\](\\([^)]\\|\\n\\)+)" text) ; inline [..](..)
        (string-match-p "\\[[^]]+\\]\\[[^]]+\\]" text)))   ; reference [..][..]
  )

;; Detect Markdown reference-style link definitions lines like:
;;   [1]: https://example.com "Title"
;;   [label]: http://example.com
(defun org-astro--markdown-link-definition-line-p (text)
  "Return non-nil if TEXT looks like a Markdown reference link definition."
  (when (and text (stringp text))
    (string-match-p "^[\t ]*\\[[^]]+\\]:[\t ]+https?://[^\t ]+.*$" (string-trim-right text))))

(defun org-astro--markdown-link-definition-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH in the buffer looks like a Markdown link definition."
  (when (and paragraph (eq (org-element-type paragraph) 'paragraph))
    (let* ((beg (org-element-property :begin paragraph))
           (end (org-element-property :end paragraph))
           (raw (and beg end (buffer-substring-no-properties beg end))))
      (and raw (org-astro--markdown-link-definition-line-p raw)))))

(defun org-astro--format-date (date-raw info)
  "Format DATE-RAW into a string suitable for Astro front matter."
  (let ((date-fmt (plist-get info :astro-date-format)))
    (format-time-string
     date-fmt
     (org-time-string-to-time date-raw))))

(defun org-astro--filename-to-alt-text (path)
  "Generate a human-readable alt text from an image PATH."
  (when (stringp path)
    (let* ((filename (file-name-sans-extension (file-name-nondirectory path)))
           ;; Replace hyphens and underscores with spaces
           (human-readable (replace-regexp-in-string "[-_]" " " filename))
           ;; Remove YYYY MM DD and long number runs
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9] [0-9][0-9] [0-9][0-9] " " " human-readable)))
      (setq cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9][0-9]+ " " " cleaned))
      (setq cleaned (org-trim cleaned))
      (capitalize cleaned))))

(defun org-astro--path-to-var-name (path)
  "Convert a file PATH to a camelCase JS variable name.
If the generated name starts with a number, it is prefixed with 'img'."
  (when (stringp path)
    (let* ((original-filename (file-name-sans-extension (file-name-nondirectory path)))
           ;; Use the sanitized filename for variable generation
           (clean-filename (org-astro--sanitize-filename original-filename))
           ;; Remove periods, underscores and other invalid characters for JS variable names
           (js-safe-filename (replace-regexp-in-string "[^a-zA-Z0-9-]" "" clean-filename))
           ;; Split on hyphens for camelCase conversion
           (parts (split-string js-safe-filename "[-]"))
           ;; Filter out empty parts
           (non-empty-parts (seq-filter (lambda (s) (> (length s) 0)) parts))
           (var-name (if (null non-empty-parts)
                         "image"
                         (concat (downcase (car non-empty-parts))
                                 (mapconcat #'capitalize (cdr non-empty-parts) "")))))
      ;; Ensure it starts with a letter (prefix with 'img' if starts with number)
      (if (and (> (length var-name) 0) (string-match-p "^[0-9]" var-name))
          (concat "img" var-name)
          var-name))))

(defun org-astro--get-task-nesting-level (heading)
  "Calculate nesting level for a TODO task by counting TODO ancestors."
  (let ((level 0)
        (current heading))
    (while (setq current (org-element-parent current))
      (when (and (eq (org-element-type current) 'headline)
                 (org-element-property :todo-keyword current))
        (setq level (1+ level))))
    level))

(defun org-astro--gen-yaml-front-matter (data)
  "Generate a YAML front-matter string from an alist DATA."
  (if (null data)
      ""
      (let ((yaml-str "---\n"))
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
            (when val
              (setq yaml-str
                    (concat yaml-str
                            (format "%s: " (symbol-name key))
                            (if (listp val)
                                (concat "\n"
                                        (mapconcat (lambda (item)
                                                     (format "- %s" item))
                                                   val "\n")
                                        "\n")
                                (format "%s\n"
                                        (if (and (stringp val)
                                                 (or (string-match-p ":" val)
                                                     (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" val))
                                                 (not (eq key 'publishDate)))
                                            ;; Quote strings that contain ':' or look like dates (avoid YAML parse issues)
                                            (format "\"%s\""
                                                    (replace-regexp-in-string
                                                     "\"" "\\\"" val))
                                            val))))))))
        (concat yaml-str "---\n"))))

(defun org-astro--get-title (tree info)
  "Return a title string from TREE/INFO, never nil."
  (or
   ;; Explicit #+TITLE first
   (let ((kw (org-element-map tree 'keyword
               (lambda (k)
                 (when (string-equal "TITLE" (org-element-property :key k)) k))
               nil 'first-match)))
     (when kw (org-element-property :value kw)))
   ;; Else first headline, safely exported
   (let ((hl (org-element-map tree 'headline 'identity nil 'first-match)))
     (when hl
       (org-astro--safe-export (org-element-property :title hl) info)))
   ;; Fallback
   "Untitled Post"))

(defun org-astro--get-excerpt (tree info)
  "Return an excerpt string from TREE/INFO, possibly empty but never nil."
  (or
   ;; Prefer explicit #+ASTRO_EXCERPT or #+EXCERPT
   (let ((kw (org-element-map tree 'keyword
               (lambda (k)
                 (when (member (org-element-property :key k)
                               '("ASTRO_EXCERPT" "EXCERPT"))
                   k))
               nil 'first-match)))
     (when kw
       (let* ((v (org-element-property :value kw)))
         (string-trim (replace-regexp-in-string "[*_/]" "" v)))))
   ;; Else first paragraph, safely exported and cleaned
   (let ((p (org-element-map tree 'paragraph 'identity nil 'first-match)))
     (when p
       (let* ((raw (org-astro--safe-export (org-element-contents p) info))
              (clean (replace-regexp-in-string "[*_/]" "" raw))
              ;; Remove image tags like ![img](path) and <img...> tags
              (no-images (replace-regexp-in-string "!\\[.*?\\]([^)]*)" "" clean))
              (no-html-images (replace-regexp-in-string "<img[^>]*>" "" no-images))
              (one   (replace-regexp-in-string "\n" " " no-html-images)))
         ;; take first sentence up to ~300 chars, else truncate with ellipsis
         (if (string-match "\`\(.\{1,300\}?[.?!]\)" one)
             (org-trim (match-string 1 one))
             (truncate-string-to-width (org-trim one) 300 nil nil "…")))))
   ;; If all else fails:
   ""))

(defun org-astro--parse-tags (info)
  "Return a list of tags from INFO, splitting on commas/whitespace/newlines."
  (let* ((tags-raw (or (plist-get info :astro-tags)
                       (plist-get info :tags)))
         (tags (when (and tags-raw (stringp tags-raw))
                 (org-split-string tags-raw "[, \t\n]+"))))
    (delq nil (mapcar #'string-trim tags))))

(defun org-astro--parse-categories (info)
  "Return a list of categories from INFO, splitting on commas/whitespace/newlines."
  (let* ((categories-raw (or (plist-get info :astro-categories)
                             (plist-get info :categories)))
         (categories (when (and categories-raw (stringp categories-raw))
                       (org-split-string categories-raw "[, \t\n]+"))))
    (delq nil (mapcar #'string-trim categories))))

(defun org-astro--get-publish-date (info)
  "Extract and format the publish date from INFO.
Falls back to the current time if no date is specified."
  (let ((date-raw (or (plist-get info :astro-publish-date)
                      (plist-get info :publish-date)
                      (plist-get info :date))))
    (if date-raw
        (org-astro--format-date date-raw info)
        (format-time-string (plist-get info :astro-date-format) (current-time)))))

(defun org-astro--get-author-image (info posts-folder)
  "Get the author image path from INFO, with defaults."
  (let ((author-image-raw (or (plist-get info :astro-author-image)
                              (plist-get info :author-image))))
    (or (and author-image-raw posts-folder
             (org-astro--process-image-path author-image-raw posts-folder "authors/"))
        org-astro-default-author-image)))

(defun org-astro--get-cover-image (info posts-folder)
  "Get the cover image path and alt text from INFO.
If no explicit cover image is specified, use the first body image as hero."
  (let* ((image-raw (or (plist-get info :astro-image)
                        (plist-get info :cover-image)))
         (image (and image-raw posts-folder
                     (let ((slug-path (if slug (concat "posts/" slug "/") "posts/")))
                       (org-astro--process-image-path image-raw posts-folder slug-path t))))
         ;; If no explicit image, try to use first body image as hero
         (body-images (or (plist-get info :astro-body-images-imports)
                          org-astro--current-body-images-imports))
         (fallback-image (when (and (not image) body-images)
                           (plist-get (car body-images) :astro-path)))
         (final-image (or image fallback-image))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and final-image (org-astro--filename-to-alt-text final-image)))))
    (list final-image image-alt)))
(defun org-astro--get-front-matter-data (tree info)
  "Build an alist of final front-matter data, applying defaults."
  (let* ((posts-folder (or (plist-get info :destination-folder)
                           (plist-get info :astro-posts-folder)))
         (title (org-astro--get-title tree info))
         ;; Generate slug from title if no explicit slug is provided
         (slug (or (plist-get info :slug)
                   (let* ((title-kw (org-element-map tree 'keyword
                                      (lambda (k)
                                        (when (string-equal "TITLE" (org-element-property :key k)) k))
                                      nil 'first-match))
                          (title-from-headline (not title-kw)))
                     ;; Only auto-generate slug if title came from headline (not from #+TITLE keyword)
                     (when title-from-headline
                       (org-astro--slugify title)))))
         (author (or (plist-get info :author) "Jay Dixit"))
         (excerpt (org-astro--get-excerpt tree info))
         (tags (org-astro--parse-tags info))
         (categories (org-astro--parse-categories info))
         (publish-date (org-astro--get-publish-date info))
         (author-image (org-astro--get-author-image info posts-folder))
         (cover-image-data (org-astro--get-cover-image info posts-folder))
         (image (car cover-image-data))
         (image-alt (cadr cover-image-data))
         (visibility (let ((v (plist-get info :visibility)))
                       (when (and v (not (string-empty-p (org-trim v))))
                         (org-trim v))))
         (status (plist-get info :status))
         (draft (when (and status (string= (downcase (org-trim status)) "draft")) "true")))
    ;; Return the alist of final data - include visibility as a string when provided
    `((title . ,title)
      ,@(when slug `((slug . ,slug)))
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      (tags . ,tags)
      (categories . ,categories)
      ,@(when visibility `((visibility . ,visibility)))
      ,@(when draft `((draft . ,draft))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX, robust to partial INFO during preflight."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link)))
    (cond
     ;; Local file image links → render Image component using imports
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (or (string-match-p "\\.\\(png\\|jpe?g\\|webp\\)$" path)
               (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" path)))
      (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                    org-astro--current-body-images-imports))
             (explicit-hero (or (plist-get info :astro-image)
                                (plist-get info :cover-image)))
             (image-imports image-imports-raw)
             (_ (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                  (message "[ox-astro][img] LINK processing path=%s, imports count=%d"
                           path (length image-imports-raw))))
             (image-data
              (when image-imports
                (or (cl-find path image-imports
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal)
                    ;; Fallback: match by sanitized filename if absolute path compare fails
                    (let* ((base (file-name-nondirectory path))
                           (base-s (org-astro--sanitize-filename (file-name-sans-extension base))))
                      (cl-find base-s image-imports
                               :key (lambda (item)
                                      (org-astro--sanitize-filename
                                       (file-name-sans-extension
                                        (file-name-nondirectory (plist-get item :path)))))
                               :test #'string-equal))))))
        (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
          (message "[ox-astro][img] LINK path=%s explicit-hero=%s imports=%s match=%s"
                   path (and explicit-hero t) (length image-imports)
                   (and image-data (plist-get image-data :var-name))))
        (cond
         ;; Suppress only the very first occurrence of the implicit hero at top
         ((and (not explicit-hero)
               image-imports-raw
               (string-equal path (plist-get (car image-imports-raw) :path))
               (not (plist-get info :astro-hero-skipped-top)))
          (plist-put info :astro-hero-skipped-top t)
          "")
         (image-data
          (let* ((var-name (plist-get image-data :var-name))
                 (alt-text (or desc (org-astro--filename-to-alt-text path) "Image")))
            (format "<Image src={%s} alt=\"%s\" />" var-name alt-text)))
         ;; Unmatched: fall back to normal Markdown link instead of dropping
         (t
          (let ((md (when (fboundp 'org-md-link)
                      (ignore-errors (org-md-link link desc info)))))
            (or md (or (org-element-property :raw-link link)
                       (concat (or type "file") ":" path))))))))
     ;; If the description is already a Markdown link, preserve it unchanged.
     ((and desc (org-astro--contains-markdown-link-p desc))
      desc)
     ;; Internal target (fuzzy) → local anchor; degrade gracefully if INFO is partial
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((resolved-title
              (condition-case _
                  (let* ((target (and (plist-get info :parse-tree)
                                      (org-export-resolve-fuzzy-link link info))))
                    (and target (org-element-property :raw-value target)))
                (error nil)))
             (text (or desc resolved-title path))
             (slug (org-astro--slugify text)))
        (format "[%s](#%s)" text slug)))

     ;; Bare URLs with no description → LinkPeek + set import flag
     ((and (null desc) (member type '("http" "https" "ftp" "mailto")))
      ;; If this bare URL is part of a Markdown reference link definition
      ;; like "[1]: https://example.com", preserve it literally.
      (let* ((parent (condition-case _
                         (org-element-property :parent link)
                       (error nil))))
        (if (and parent (org-astro--markdown-link-definition-paragraph-p parent))
            ;; Return the raw URL so the whole line exports as "[x]: url"
            (or (org-element-property :raw-link link)
                (concat type ":" path))
            ;; Otherwise, emit LinkPeek + mark for import
            (progn
              (setf (plist-get info :astro-uses-linkpeek) t)
              (format "<LinkPeek href=\"%s\"></LinkPeek>"
                      (or (org-element-property :raw-link link)
                          (concat type ":" path)))))))

     ;; Everything else → prefer org's MD translator; fall back to plain Markdown
     (t
      (let ((md (when (fboundp 'org-md-link)
                  (ignore-errors (org-md-link link desc info)))))
        (or md
            (let* ((raw (or (org-element-property :raw-link link)
                            (concat type ":" path)))
                   (text (or desc raw)))
              (format "[%s](%s)" text raw))))))))

(defun org-astro-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element into fenced Markdown format."
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        (when (and (member lang '("user" "prompt" "quote")) (string-match-p "---" code))
          (setq code (replace-regexp-in-string "---" "—" code)))
        ;; Trim trailing newlines/whitespace to prevent extra space at the end.
        (setq code (string-trim-right code))
        (format "```%s\n%s\n```" (or lang "") code))
      ;; Fallback to simple fenced code block
      (let* ((lang (org-element-property :language src-block))
             (code (org-element-property :value src-block)))
        (format "```%s\n%s\n```" (or lang "") (org-trim code)))))

(defun org-astro-heading (heading contents info)
  (let ((todo-keyword (org-element-property :todo-keyword heading)))
    (if todo-keyword
        ;; task style
        (let* ((info-copy (copy-sequence info))
               (_ (plist-put info-copy :with-smart-quotes nil))
               (title (org-astro--safe-export (org-element-property :title heading) info-copy))
               (nesting-level (org-astro--get-task-nesting-level heading))
               (indent (make-string (* 2 nesting-level) ? ))
               (donep (member todo-keyword org-done-keywords))
               (checkbox (if donep "[x]" "[ ]"))
               (trimmed-contents (if contents (org-trim contents) ""))
               (indented-contents
                (if (> (length trimmed-contents) 0)
                    (let ((content-indent (make-string (+ 2 (* 2 nesting-level)) ? )))
                      (concat "\n" content-indent
                              (replace-regexp-in-string
                               "\n"
                               (concat "\n" content-indent)
                               trimmed-contents)))
                    "")))
          (format "%s- %s %s%s\n" indent checkbox title indented-contents))
        ;; regular heading
        (let* ((info-copy (copy-sequence info))
               (_ (plist-put info-copy :with-smart-quotes nil))
               (title (org-astro--safe-export (org-element-property :title heading) info-copy))
               (level (+ (org-element-property :level heading)
                         (or (plist-get info :headline-offset) 0)))
               (level (min (max level 1) 6))
               (header (concat (make-string level ?#) " " title)))
          (concat header "\n\n" (or contents ""))))))

(defun org-astro--handle-broken-image-paragraph (paragraph info)
  "Handle a paragraph containing a broken image path with subscripts."
  (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                org-astro--current-body-images-imports))
         ;; Exclude first image if it's being used as the implicit hero
         (explicit-hero (or (plist-get info :astro-image)
                            (plist-get info :cover-image)))
         (image-imports (if (and (not explicit-hero) image-imports-raw)
                            (cdr image-imports-raw)
                            image-imports-raw))
         (paragraph-context (org-element-interpret-data paragraph))
         (matching-import nil))


    ;; Try to find a matching imported image by comparing filenames
    (when image-imports
      (dolist (import image-imports)
        (let* ((import-file-path (plist-get import :path))
               (import-filename (file-name-nondirectory import-file-path)))
          ;; Check if the paragraph context contains this filename (even broken up)
          (when (and import-filename
                     (string-match-p (regexp-quote (file-name-sans-extension import-filename))
                                     paragraph-context))
            (setq matching-import import)))))

    (if matching-import
        ;; Generate Image component for matched import
        (let* ((var-name (plist-get matching-import :var-name))
               (matched-path (plist-get matching-import :path))
               (alt-text (or (org-astro--filename-to-alt-text matched-path) "Image")))
          (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
        ;; No match found - remove the broken paragraph
        "")))


(defun org-astro-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element.
If the paragraph is a raw image path, convert it to an <img> tag.
Otherwise, use the default Markdown paragraph transcoding."
  ;; Debug: Log paragraphs that might contain raw image paths
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((children (org-element-contents paragraph))
           (child (and (= 1 (length children)) (car children))))
      (when (and child (eq 'plain-text (org-element-type child)))
        (let* ((raw-text (org-element-property :value child))
               (text (when (stringp raw-text) (org-trim raw-text))))
          (when (and text (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text))
            (org-astro--dbg-log info "PARAGRAPH processing raw image path: %s" text))))))
  (let* ((children (org-element-contents paragraph))
         (child (and (= 1 (length children)) (car children)))
         (is-image-path nil)
         path)
    (when (and child (eq 'plain-text (org-element-type child)))
      (let* ((raw-text (org-element-property :value child))
             (text (when (stringp raw-text) (org-trim raw-text))))
        (when (and text
                   (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text))
          (setq is-image-path t)
          (setq path text))))
    (if is-image-path
        (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                      org-astro--current-body-images-imports))
               ;; Exclude first image if it's being used as the implicit hero
               (explicit-hero (or (plist-get info :astro-image)
                                  (plist-get info :cover-image)))
               (image-imports (if (and (not explicit-hero) image-imports-raw)
                                  (cdr image-imports-raw)
                                  image-imports-raw))
               (image-data (when image-imports
                             (or (cl-find path image-imports
                                          :key (lambda (item) (plist-get item :path))
                                          :test #'string-equal)
                                 ;; Fallback by sanitized filename (handles underscore→hyphen sanitation)
                                 (let* ((base (file-name-nondirectory path))
                                        (base-s (org-astro--sanitize-filename (file-name-sans-extension base))))
                                   (cl-find base-s image-imports
                                            :key (lambda (item)
                                                   (org-astro--sanitize-filename
                                                    (file-name-sans-extension
                                                     (file-name-nondirectory (plist-get item :path)))))
                                            :test #'string-equal))))))
          (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
            (message "[ox-astro][img] PARA path=%s explicit-hero=%s imports=%s match=%s"
                     path (and explicit-hero t) (length image-imports)
                     (and image-data (plist-get image-data :var-name))))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
              ;; Fallback: if image wasn't processed by the filter, just output the original contents.
              contents))
        ;; Check if this paragraph contains broken image path (subscripts)
        (let ((paragraph-context (org-element-interpret-data paragraph)))
          (if (string-match-p "/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)" paragraph-context)
              ;; This paragraph contains a broken image path - try to handle it
              (org-astro--handle-broken-image-paragraph paragraph info)
              ;; Regular paragraph
              (org-md-paragraph paragraph contents info))))))


(defun org-astro-plain-text (text info)
  "Transcode a plain-text element.
If the text contains raw image paths on their own lines, convert them to <img> tags.
If the text contains raw URLs on their own lines, convert them to LinkPeek components."
  (let* ((lines (split-string text "\n"))
         (image-imports (or (plist-get info :astro-body-images-imports)
                            org-astro--current-body-images-imports))
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line)))
               (if (org-astro--contains-markdown-link-p line)
                   ;; Preserve Markdown link formatting unchanged
                   line
                   (cond
                    ;; Raw image path (trust imports rather than filesystem)
                    ((and trimmed-line
                          (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" trimmed-line)
                              (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" trimmed-line)))
                     (let ((image-data (when image-imports
                                         (cl-find trimmed-line image-imports
                                                  :key (lambda (item) (plist-get item :path))
                                                  :test #'string-equal))))
                       (if image-data
                           (let ((var-name (plist-get image-data :var-name))
                                 (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                             (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                           ;; Fallback: if image wasn't processed by the filter, output as plain text.
                           line)))
                    ;; Remote image URL
                    ((and trimmed-line
                          (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" trimmed-line))
                     (let ((image-data (when image-imports
                                         (cl-find trimmed-line image-imports
                                                  :key (lambda (item) (plist-get item :path))
                                                  :test #'string-equal))))
                       (if image-data
                           (let ((var-name (plist-get image-data :var-name))
                                 (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                             (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                           ;; Fallback: if image wasn't processed by the filter, output as plain text.
                           line)))
                    ;; Regular remote URL (non-image)
                    ((and trimmed-line
                          (string-match-p "^https?://[^[:space:]]+$" trimmed-line))
                     (setq has-linkpeek t)
                     (format "<LinkPeek href=\"%s\"></LinkPeek>" trimmed-line))
                    ;; Regular line
                    (t line)))))
           lines)))
    ;; Store LinkPeek usage in info for import generation
    (when has-linkpeek
      (plist-put info :astro-uses-linkpeek t))
    (mapconcat 'identity processed-lines "\n")))


(defun org-astro-subscript (subscript contents info)
  "Handle subscript elements, removing ones that are part of broken image paths."
  (let* ((parent (org-element-property :parent subscript))
         (parent-context (when parent (org-element-interpret-data parent))))
    (if (and parent-context
             (string-match-p "/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)" parent-context))
        ;; This subscript is part of a broken image path - remove it entirely
        ;; The raw image collection will handle the proper image processing
        ""
        ;; Regular subscript - use default markdown handling
        (format "_%s_" (or contents "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--collect-images-from-tree (tree)
  "Collect all image paths from the parse TREE.
This includes both `[[file:...]]` links and raw image paths on their own line."
  (let (images)
    ;; 1. Collect from `link` elements
    (org-element-map tree 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (string= type "file")
                     (string-match-p "\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)$" path))
            (push path images)))))
    ;; 2. Collect from raw paths in all plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element)
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n"))))
          (dolist (line lines)
            (let ((text (org-trim line)))
              (when (and text
                         ;; Match absolute paths OR paths containing "assets/images"
                         (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text)
                             (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" text)))
                (push text images)))))))
    ;; 3. Collect from paragraphs that contain subscript elements (broken up image paths)
    (org-element-map tree 'paragraph
      (lambda (paragraph)
        (let ((reconstructed-path (org-astro--extract-image-path-from-paragraph paragraph)))
          (when (and reconstructed-path
                     (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" reconstructed-path)
                         (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" reconstructed-path)))
            (push reconstructed-path images)))))
    ;; 4. Collect remote image URLs from plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element)
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n"))))
          (dolist (line lines)
            (let ((text (org-trim line)))
              (when (and text
                         (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" text))
                (push text images)))))))
    ;; Return a list with no duplicates
    (delete-dups (nreverse images))))

(defun org-astro--collect-raw-image-paths ()
  "Collect image paths from raw buffer content, before org-mode parsing.
This catches paths with underscores that would be broken by subscript parsing."
  (let (images)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
        (let ((path (string-trim (match-string 0))))
          (push path images))))
    images))

(defun org-astro--extract-image-path-from-paragraph (paragraph)
  "Extract a potential image path from a PARAGRAPH that may contain subscript elements."
  (let ((raw-content (org-element-interpret-data paragraph)))
    ;; Look for patterns like /Users/jay/Downloads/file_name.webp that may have been broken by subscripts
    (when (string-match "/[^[:space:]<>]*\\.\\(webp\\|png\\|jpe?g\\)" raw-content)
      (let ((potential-path (match-string 0 raw-content)))
        ;; Clean up any HTML artifacts
        (setq potential-path (replace-regexp-in-string "<[^>]*>" "" potential-path))
        ;; Clean up any whitespace
        (setq potential-path (string-trim potential-path))
        ;; If it looks like a valid absolute path, return it
        (when (string-match-p "^/" potential-path)
          potential-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--update-image-path-in-buffer (old-path new-path)
  "Replace OLD-PATH with NEW-PATH in the current buffer.
This updates:
- Org links [[file:OLD]][DESC] → [[file:NEW]][DESC]
- Bare org links [[OLD]][DESC] → [[NEW]][DESC]
- Raw lines containing only the path (ignoring surrounding whitespace)."
  (message "DEBUG: Starting buffer update - old: %s -> new: %s" old-path new-path)
  (message "DEBUG: Current buffer: %s (file: %s)" (buffer-name) (buffer-file-name))
  (message "DEBUG: Buffer modified: %s, read-only: %s" (buffer-modified-p) buffer-read-only)

  (save-excursion
    (goto-char (point-min))
    (let ((changes-made nil)
          (buffer-content-preview (buffer-substring (point-min) (min (+ (point-min) 200) (point-max)))))
      (message "DEBUG: Buffer preview: %s..." buffer-content-preview)

      ;; 1) Update [[file:OLD]] and [[file:OLD][DESC]]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\(\\[[^]]*\\]\\)?\\]" nil t)
        (let ((match-beg (match-beginning 1))
              (match-end (match-end 1))
              (captured (match-string 1)))
          (message "DEBUG: Found file link: %s" captured)
          (when (string-equal captured old-path)
            (message "DEBUG: Updating file link match")
            (goto-char match-beg)
            (delete-region match-beg match-end)
            (insert new-path)
            (setq changes-made t))))

      ;; 2) Update bare [[OLD]] and [[OLD][DESC]] (Org treats these as file links too)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(/[^]]+\\)\\]\\(\\[[^]]*\\]\\)?\\]" nil t)
        (let ((match-beg (match-beginning 1))
              (match-end (match-end 1))
              (captured (match-string 1)))
          (message "DEBUG: Found bare link: %s" captured)
          (when (string-equal captured old-path)
            (message "DEBUG: Updating bare link match")
            (goto-char match-beg)
            (delete-region match-beg match-end)
            (insert new-path)
            (setq changes-made t))))

      ;; 3) Update raw lines containing only the path (allowing whitespace)
      (goto-char (point-min))
      (let ((search-pattern (format "^[\t ]*%s[\t ]*$" (regexp-quote old-path))))
        (message "DEBUG: Searching for raw pattern: %s" search-pattern)
        (while (re-search-forward search-pattern nil t)
          (message "DEBUG: Found raw path match at line %d" (line-number-at-pos))
          (replace-match new-path t t)
          (setq changes-made t)))

      (message "DEBUG: Buffer update complete. Changes made: %s" changes-made)
      changes-made)))

(defun org-astro--update-image-path-in-file (file old-path new-path)
  "Open FILE, replace OLD-PATH with NEW-PATH using the same rules as buffer updater, and save.
Returns non-nil if any changes were made."
  (when (and (stringp file) (file-exists-p file)
             (stringp old-path) (stringp new-path))
    (with-current-buffer (find-file-noselect file)
      (let ((changed (org-astro--update-image-path-in-buffer old-path new-path)))
        (when changed (save-buffer))
        changed))))

(defun org-astro--update-source-buffer-image-path (old-path new-path)
  "Update image path in the original source buffer, not the export copy.
This function finds the source buffer and modifies it directly."
  (message "DEBUG: Looking for source buffer to update path %s -> %s" old-path new-path)

  ;; Strategy 1: Check if current buffer has a file name and is writable
  (let ((source-buffer nil))
    (cond
     ;; Current buffer is the source file
     ((and (buffer-file-name) (not buffer-read-only))
      (message "DEBUG: Using current buffer as source: %s" (buffer-name))
      (setq source-buffer (current-buffer)))

     ;; Try to find source buffer by examining all buffers
     (t
      (message "DEBUG: Current buffer (%s) is not suitable, searching for source buffer..." (buffer-name))
      (message "DEBUG: Current buffer file: %s, read-only: %s" (buffer-file-name) buffer-read-only)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (let ((buf-file (buffer-file-name))
                (buf-readonly buffer-read-only))
            (message "DEBUG: Checking buffer %s (file: %s, readonly: %s)" (buffer-name) buf-file buf-readonly)
            (when (and buf-file
                       (not buf-readonly)
                       (string-match-p "\\.org$" buf-file)
                       ;; Check if this buffer contains the image path (raw or bracketed)
                       (save-excursion
                         (goto-char (point-min))
                         (or (search-forward old-path nil t)
                             (search-forward (format "[[%s]]" old-path) nil t))))
              (message "DEBUG: Found source buffer: %s (%s)" (buffer-name) buf-file)
              (setq source-buffer buf)
              (return)))))))

    ;; Now use the source-buffer within the same let binding
    (if source-buffer
        (progn
          (message "DEBUG: Updating source buffer: %s" (buffer-name source-buffer))
          (with-current-buffer source-buffer
            (let ((changes-made (org-astro--update-image-path-in-buffer old-path new-path)))
              (when changes-made
                (message "DEBUG: Saving source buffer after changes")
                (save-buffer))
              changes-made)))
        (message "DEBUG: WARNING - Could not find source buffer to update!"))))

(defun org-astro--get-assets-folder (posts-folder sub-dir)
  "Get the assets folder based on POSTS-FOLDER and SUB-DIR."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; Go up from content/blog to src
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir))))))
      (expand-file-name (concat "assets/images/" sub-dir) src-dir))))

;; ------------------------------------------------------------------
;; Image path suggestions block (for manual replacement workflow)
;; ------------------------------------------------------------------

;; Preprocessing: wrap raw absolute image paths in org link brackets [[...]]
(defun org-astro--wrap-raw-image-path-lines-in-region (beg end)
  "Within BEG..END, wrap raw absolute image path lines with Org link brackets.
Only wraps lines that are just an absolute image path (png/jpg/jpeg/webp),
and are not already an Org link. Returns number of lines changed."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line-content (buffer-substring-no-properties line-start line-end)))
            (cond
             ;; Skip lines that already have brackets
             ((string-match "\\[\\[.*\\]\\]" line-content)
              (forward-line 1))
             ;; Process lines that are only raw image paths
             ((string-match "^\\s-*\\(/[^[:space:]]*\\.\\(?:png\\|jpe?g\\|webp\\)\\)\\s-*$" line-content)
              (let ((path (match-string 1 line-content)))
                (delete-region line-start line-end)
                (insert (format "[[%s]]" path))
                (setq count (1+ count))
                (forward-line 1)))
             ;; Skip other lines
             (t (forward-line 1)))))))
    count))

(defun org-astro--persist-wrap-raw-image-lines (file)
  "Open FILE, wrap raw absolute image path lines with [[...]], save, and return count of lines changed."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (let ((cnt (org-astro--wrap-raw-image-path-lines-in-region (point-min) (point-max))))
          (when (> cnt 0)
            (save-buffer))
          cnt)))))


(defun org-astro--generate-image-paths-comment-block (items)
  "Generate a comment block with suggested image path replacements.
ITEMS is a list of plists containing :path (old), :target-path (abs new), :astro-path (alias)."
  (let ((lines (list "# BEGIN ASTRO IMAGE PATH SUGGESTIONS"
                     "# Suggested replacements (old → new). Use alias for MDX."
                     (format "# Generated: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
                     "#")))
    (dolist (it items)
      (let* ((old (plist-get it :path))
             (new (or (plist-get it :target-path) ""))
             (alias (or (plist-get it :astro-path) "")))
        (push (format "# - old: %s" old) lines)
        (push (format "#   new: %s" new) lines)
        (push (format "#   alias: %s" alias) lines)
        (push "#" lines)))
    (setq lines (nreverse lines))
    (mapconcat #'identity (append lines (list "# END ASTRO IMAGE PATH SUGGESTIONS")) "\n")))

(defun org-astro--insert-or-replace-suggestions-block (block-text)
  "Insert or replace the image suggestions comment BLOCK-TEXT near the top."
  (save-excursion
    (goto-char (point-min))
    (let* ((limit (save-excursion (or (re-search-forward "^\\*" nil t) (point-max))))
           (begin-marker (progn
                           (save-restriction
                             (narrow-to-region (point-min) limit)
                             (goto-char (point-min))
                             (re-search-forward "^# BEGIN ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
           (end-marker (when begin-marker
                         (save-excursion
                           (re-search-forward "^# END ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
           (insert-point
            (unless begin-marker
              ;; Compute where to insert: after roam preamble and keyword/comment block
              (goto-char (point-min))
              ;; Skip properties block
              (when (looking-at-p "^:PROPERTIES:")
                (forward-line 1)
                (while (and (< (point) limit)
                            (not (looking-at-p "^:END:")))
                  (forward-line 1))
                (when (looking-at-p "^:END:") (forward-line 1)))
              ;; Skip roam preamble lines
              (let ((roam-anchor (save-excursion
                                   (let ((last-pos nil))
                                     (save-restriction
                                       (narrow-to-region (point-min) limit)
                                       (goto-char (point-min))
                                       (while (re-search-forward "^-[ \t]+\(Links\|Source\) ::[ \t]*$" nil t)
                                         (setq last-pos (line-end-position))))
                                     (when last-pos
                                       (goto-char last-pos)
                                       (forward-line 1)
                                       (point))))))
                (if roam-anchor
                    (goto-char roam-anchor)
                    ;; Else skip existing keywords/comments/blank
                    (while (and (< (point) limit)
                                (or (looking-at-p "^#\\+") (looking-at-p "^#\\s-") (looking-at-p "^\\s-*$")))
                      (forward-line 1))))
              (point))))
      (if begin-marker
          (progn
            (goto-char begin-marker)
            (beginning-of-line)
            (let ((start (point)))
              (when end-marker
                (goto-char end-marker)
                (end-of-line)
                (forward-char 1))
              (delete-region start (point)))
            (insert block-text "\n"))
          ;; Insert new block
          (goto-char insert-point)
          (unless (or (bobp) (looking-at-p "^\\s-*$")) (insert "\n"))
          (insert block-text "\n")))))

(defun org-astro--upsert-image-paths-comment-in-current-buffer (items)
  "Create or update the suggestions comment block in the current buffer."
  (let ((block (org-astro--generate-image-paths-comment-block items)))
    (org-astro--insert-or-replace-suggestions-block block)))

(defun org-astro--upsert-image-paths-comment (items)
  "Find the source Org buffer and upsert the suggestions comment block."
  (message "DEBUG: Upserting image suggestions block...")
  (let ((source-buffer nil))
    (cond
     ((and (buffer-file-name) (not buffer-read-only))
      (setq source-buffer (current-buffer)))
     (t
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (let ((bf (buffer-file-name)))
            (when (and bf (not buffer-read-only)
                       (string-match-p "\\.org$" bf))
              (setq source-buffer buf)
              (cl-return)))))))
    (when source-buffer
      (with-current-buffer source-buffer
        (org-astro--upsert-image-paths-comment-in-current-buffer items)
        (save-buffer)))))

(defun org-astro--upsert-image-paths-comment-into-file (file items)
  "Open FILE and upsert the image suggestions comment block, then save."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-astro--upsert-image-paths-comment-in-current-buffer items)
      (save-buffer))))

;; ------------------------------------------------------------------
;; Apply suggested image path replacements (manual, explicit action)
;; ------------------------------------------------------------------

(defun org-astro--parse-image-suggestions ()
  "Parse the suggestions block and return a list of plist items:
Each item has :old, :new, and :alias keys.
Returns nil if no suggestions block is found."
  (save-excursion
    (goto-char (point-min))
    (let* ((begin (re-search-forward "^# BEGIN ASTRO IMAGE PATH SUGGESTIONS$" nil t))
           (end (when begin (re-search-forward "^# END ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
      (when (and begin end)
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (let (items current-old current-new current-alias)
            (while (re-search-forward "^# - old: \(.*\)$" nil t)
              (setq current-old (org-trim (match-string 1)))
              (when (re-search-forward "^#   new: \(.*\)$" nil t)
                (setq current-new (org-trim (match-string 1))))
              (when (re-search-forward "^#   alias: \(.*\)$" nil t)
                (setq current-alias (org-trim (match-string 1))))
              (when (and current-old (or current-new current-alias))
                (push (list :old current-old :new current-new :alias current-alias) items))
              (setq current-old nil current-new nil current-alias nil))
            (nreverse items)))))))

(defun org-astro-apply-image-path-replacements (&optional use-alias)
  "Apply suggested image path replacements in the current buffer.
When USE-ALIAS is non-nil (interactive prefix), use :alias paths; otherwise use :new absolute paths."
  (interactive "P")
  (let* ((items (org-astro--parse-image-suggestions))
         (count 0)
         (mode (if use-alias :alias :new)))
    (if (not items)
        (message "No image suggestions block found.")
        (save-excursion
          (save-restriction
            (widen)
            (dolist (it items)
              (let* ((old (plist-get it :old))
                     (rep (plist-get it mode)))
                (when (and old rep (not (string-empty-p rep)))
                  (setq count (+ count (if (org-astro--update-image-path-in-buffer old rep) 1 0))))))
            (when (> count 0)
              (save-buffer)))
          (message "Applied replacements for %d image(s) using %s paths." count (if use-alias "alias" "absolute")))))
  )

(defun org-astro-apply-image-path-replacements-in-file (file &optional use-alias)
  "Open FILE, apply replacements from its suggestions block, and save.
When USE-ALIAS is non-nil, use :alias paths; otherwise use :new."
  (interactive "fOrg file: \nP")
  (with-current-buffer (find-file-noselect file)
    (org-astro-apply-image-path-replacements use-alias)))


(defun org-astro--sanitize-filename (filename)
  "Sanitize FILENAME by replacing underscores with hyphens and removing problematic characters."
  (let ((clean-name filename))
    ;; Replace underscores with hyphens to avoid subscript parsing issues
    (setq clean-name (replace-regexp-in-string "_" "-" clean-name))
    ;; Remove or replace other potentially problematic characters
    (setq clean-name (replace-regexp-in-string "[^a-zA-Z0-9.-]" "-" clean-name))
    ;; Remove multiple consecutive hyphens
    (setq clean-name (replace-regexp-in-string "--+" "-" clean-name))
    clean-name))

(defun org-astro--download-remote-image (url posts-folder sub-dir)
  "Download remote image from URL to assets folder.
Returns the local file path if successful, nil otherwise."
  (when (and url posts-folder (string-match-p "^https?://" url))
    (let* ((url-parts (url-generic-parse-url url))
           (path (url-filename url-parts))
           ;; Extract filename from URL path, handling query parameters
           (raw-filename (if (string-match "\\([^/?]+\\)\\(\\?.*\\)?$" path)
                             (match-string 1 path)
                             "downloaded-image"))
           ;; Ensure we have an image extension
           (filename (if (string-match-p "\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)$" raw-filename)
                         raw-filename
                         (concat raw-filename ".jpg")))
           (clean-filename (org-astro--sanitize-filename filename))
           (assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
           (target-path (when assets-folder
                          (expand-file-name clean-filename assets-folder))))

      (when (and target-path assets-folder)
        ;; Create assets directory if it doesn't exist
        (make-directory assets-folder t)

        ;; Download the image
        (condition-case err
            (progn
              (message "Downloading remote image: %s" url)
              (org-astro--dbg-log nil "REMOTE downloading: %s" url)
              (url-copy-file url target-path t)
              (when (file-exists-p target-path)
                (message "Successfully downloaded: %s -> %s" url target-path)
                (org-astro--dbg-log nil "REMOTE downloaded: %s -> %s" url clean-filename)
                target-path))
          (error
           (message "Failed to download image %s: %s" url err)
           (org-astro--dbg-log nil "REMOTE failed: %s - %s" url err)
           nil))))))

(defun org-astro--process-image-path (image-path posts-folder sub-dir &optional update-buffer)
  "Process IMAGE-PATH for POSTS-FOLDER, copying local files or downloading remote URLs to SUB-DIR.
Returns the processed path suitable for Astro imports.
If UPDATE-BUFFER is non-nil, updates the current buffer to point to the new path."
  (message "DEBUG: Processing image - path: %s, posts-folder: %s, sub-dir: %s, update-buffer: %s"
           image-path posts-folder sub-dir update-buffer)

  (when (and image-path posts-folder)
    (let ((image-path (substring-no-properties image-path)))
      (cond
       ;; Handle images already in the assets folder - just return the alias path
       ((let ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir)))
          (message "DEBUG: Checking if in assets - assets-folder: %s, image-path: %s"
                   assets-folder image-path)
          (and assets-folder
               (string-match-p (regexp-quote (expand-file-name assets-folder))
                               (expand-file-name image-path))))
        (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
               (relative-path (file-relative-name image-path (expand-file-name assets-folder)))
               (filename (file-name-nondirectory image-path))
               (result (concat "~/assets/images/" sub-dir filename)))
          (message "DEBUG: Image already in assets folder - returning: %s" result)
          result))

       ;; Handle remote URLs (both full https:// and protocol-relative //)
       ((let ((is-remote (or (string-match-p "^https?://" image-path)
                             (and (string-match-p "^//" image-path)
                                  (string-match-p "\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)" image-path)))))
          (message "DEBUG: Checking remote URL for %s. Is remote: %s" image-path is-remote)
          is-remote)
        (let* ((full-url (if (string-match-p "^//" image-path)
                             (concat "https:" image-path)
                             image-path))
               (downloaded-path (org-astro--download-remote-image full-url posts-folder sub-dir)))
          (when downloaded-path
            (let* ((clean-filename (file-name-nondirectory downloaded-path))
                   (result (concat "~/assets/images/" sub-dir clean-filename)))
              ;; Update the buffer to replace remote URL with local path
              ;; For protocol-relative URLs, we need to find the original https:// version in the buffer
              (when update-buffer
                (let ((original-url (if (string-match-p "^//" image-path)
                                        ;; Try to find the original https:// version in the buffer
                                        full-url
                                        ;; Use the path as-is if it already has protocol
                                        image-path)))
                  (message "DEBUG: Updating buffer - remote URL %s -> local path %s" original-url downloaded-path)
                  (org-astro--update-source-buffer-image-path original-url downloaded-path)))
              (message "DEBUG: Remote image processed - returning astro path: %s" result)
              result))))

       ;; Handle local files
       (t
        (message "DEBUG: Handling local file: %s" image-path)
        (message "DEBUG: File exists check: %s" (file-exists-p image-path))
        (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
               (original-filename (file-name-nondirectory image-path))
               (clean-filename (org-astro--sanitize-filename original-filename))
               (target-path (when assets-folder (expand-file-name clean-filename assets-folder))))

          (message "DEBUG: Assets folder: %s" assets-folder)
          (message "DEBUG: Target path: %s" target-path)
          (message "DEBUG: About to check conditions - target-path: %s, file-exists: %s"
                   (and target-path t) (file-exists-p image-path))

          (when (and target-path (file-exists-p image-path))
            ;; Create assets directory if it doesn't exist
            (when assets-folder
              (make-directory assets-folder t))
            ;; Copy the file
            (condition-case err
                (progn
                  (copy-file image-path target-path t)
                  (message "DEBUG: Successfully copied %s to %s" image-path target-path))
              (error (message "Failed to copy image %s: %s" image-path err)))

            ;; Update the buffer if requested (even if file already exists from previous run)
            (when update-buffer
              (message "DEBUG: Attempting buffer update...")
              (org-astro--update-source-buffer-image-path image-path target-path))

            ;; Return the alias path for imports
            (when (file-exists-p target-path)
              (let ((result (concat "~/assets/images/" sub-dir clean-filename)))
                (message "DEBUG: Returning astro path: %s" result)
                result)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-table (table contents info)
  "Transcode a TABLE element from Org to Markdown.
CONTENTS holds the contents of the table.  INFO is a plist
holding contextual information."
  (let* ((rows (org-element-map table 'table-row
                 (lambda (row)
                   (when (eq (org-element-property :type row) 'standard)
                     row))))
         (table-rows (mapcar (lambda (row)
                               (org-astro-table-row row nil info))
                             rows)))
    (when table-rows
      (let ((first-row (car table-rows))
            (remaining-rows (cdr table-rows)))
        (concat
         first-row "\n"
         (org-astro--table-separator-row (car rows) info) "\n"
         (when remaining-rows
           (concat (mapconcat #'identity remaining-rows "\n") "\n")))))))

(defun org-astro-table-row (table-row _contents info)
  "Transcode a TABLE-ROW element from Org to Markdown.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((cells (org-element-map table-row 'table-cell
                    (lambda (cell)
                      (org-astro-table-cell cell nil info))))
           (row-content (mapconcat #'identity cells " | ")))
      (concat "| " row-content " |"))))

(defun org-astro-table-cell (table-cell _contents info)
  "Transcode a TABLE-CELL element from Org to Markdown.
CONTENTS is the cell contents.  INFO is a plist used as a
communication channel."
  (let ((cell-contents (org-element-contents table-cell)))
    (if cell-contents
        (org-trim (org-export-data cell-contents info))
        "")))

(defun org-astro--table-separator-row (header-row info)
  "Generate a Markdown table separator row based on HEADER-ROW.
INFO is a plist used as a communication channel."
  (let* ((cells (org-element-map header-row 'table-cell #'identity))
         (separators (mapcar (lambda (_cell) "---") cells)))
    (concat "| " (mapconcat #'identity separators " | ") " |")))

(defun org-astro-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element.
Handle GALLERY blocks specially by converting them to ImageGallery components."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ;; Handle GALLERY blocks
     ((string-equal block-type "GALLERY")
      (org-astro--dbg-log info "Processing GALLERY block")
      (let* ((image-imports (plist-get info :astro-body-images-imports))
             (gallery-id (concat "gallery-" (number-to-string (random 10000))))
             (gallery-items nil))
        ;; Debug: log what's in image-imports
        (org-astro--dbg-log info "GALLERY image-imports: %s" 
                            (mapcar (lambda (item) (plist-get item :path)) image-imports))
        ;; First, extract image links from the block contents
        (org-element-map special-block 'link
          (lambda (link)
            (when (and (string-equal (org-element-property :type link) "file")
                       (let ((path (org-element-property :path link)))
                         (and path (string-match-p "\\.\\(png\\|jpe?g\\|webp\\)$" path))))
              (let* ((path (org-element-property :path link))
                     ;; Try to find the import data by matching the sanitized filename
                     (filename (file-name-nondirectory path))
                     (sanitized-filename (org-astro--sanitize-filename filename))
                     (import-data (cl-find-if 
                                   (lambda (item) 
                                     (string-equal sanitized-filename 
                                                   (file-name-nondirectory (plist-get item :path))))
                                   image-imports))
                     (var-name (when import-data (plist-get import-data :var-name)))
                     (alt-text (org-astro--filename-to-alt-text path)))
                (org-astro--dbg-log info "GALLERY checking link path: %s (sanitized: %s), found in imports: %s" 
                                    path sanitized-filename (if import-data "YES" "NO"))
                (when var-name
                  (org-astro--dbg-log info "GALLERY found image: %s -> %s" path var-name)
                  (push (format "    { src: %s, alt: \"%s\" }" var-name alt-text) gallery-items))))))
        
        ;; Also extract raw image paths from the block region
        (let ((beg (org-element-property :begin special-block))
              (end (org-element-property :end special-block)))
          (when (and beg end)
            (save-excursion
              (save-restriction
                (narrow-to-region beg end)
                (goto-char (point-min))
                ;; Look for raw image paths (absolute paths ending in image extensions)
                (while (re-search-forward "^\\s-*/[^[:space:]]+\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
                  (let* ((raw-path (string-trim (match-string 0)))
                         ;; Try to find the import data by matching the sanitized filename
                         (filename (file-name-nondirectory raw-path))
                         (sanitized-filename (org-astro--sanitize-filename filename))
                         (import-data (cl-find-if 
                                       (lambda (item) 
                                         (string-equal sanitized-filename 
                                                       (file-name-nondirectory (plist-get item :path))))
                                       image-imports))
                         (var-name (when import-data (plist-get import-data :var-name)))
                         (alt-text (org-astro--filename-to-alt-text raw-path)))
                    (org-astro--dbg-log info "GALLERY checking raw path: %s (sanitized: %s), found in imports: %s" 
                                        raw-path sanitized-filename (if import-data "YES" "NO"))
                    (when var-name
                      (org-astro--dbg-log info "GALLERY found image: %s -> %s" raw-path var-name)
                      (push (format "    { src: %s, alt: \"%s\" }" var-name alt-text) gallery-items))))))))
        
        ;; Generate ImageGallery component
        (org-astro--dbg-log info "GALLERY generating component with %d images" (length gallery-items))
        (if gallery-items
            (concat "<ImageGallery\n"
                    "  images={[\n"
                    (mapconcat #'identity (reverse gallery-items) ",\n")
                    "\n  ]}\n"
                    "  galleryId=\"" gallery-id "\"\n"
                    "/>")
            ;; Fallback if no images found
            contents)))
     ;; Default: use standard markdown export
     (t (org-md-special-block special-block contents info)))))

(defun org-astro--collect-raw-images-from-tree-region (tree)
  "Collect raw image paths by scanning the buffer region of a parse TREE.
This is more robust for narrowed subtrees than relying on `plain-text` parsing."
  (let (images)
    (let ((beg (org-element-property :begin tree))
          (end (org-element-property :end tree)))
      (when (and beg end)
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
              (let ((path (string-trim (match-string 0))))
                (when (file-exists-p path)
                  (push path images))))))))
    (nreverse images)))

(provide 'ox-astro-helpers)
;;; ox-astro-helpers.el ends here
