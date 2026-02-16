;;; ox-astro-transcode.el --- Transcoding functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:
(require 'org)
(require 'org-element)
(require 'ox-md)
(require 'subr-x)
(require 'cl-lib)

(require 'ox-astro-helpers-core)
(require 'ox-astro-export-helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-underline (underline contents _info)
  "Transcode UNDERLINE without producing MDX spans.
- If the underline is just placeholder underscores (3+), emit the underscores.
- Otherwise, emit plain contents/value with no additional markup."
  (let* ((begin (org-element-property :begin underline))
         (end (org-element-property :end underline))
         (raw (and begin end
                   (buffer-substring-no-properties begin end)))
         (trimmed (and raw (string-trim-right raw)))
         (placeholder (and trimmed
                           (string-match "^_\\{3,\\}$" trimmed)
                           trimmed))
         (text (or contents (org-element-property :value underline))))
    (cond
     (placeholder placeholder)
     (text text)
     (t ""))))

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX, robust to partial INFO during preflight."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (or (org-element-property :raw-link link)
                       (and type path (concat type ":" path))))
         (parent (condition-case _
                     (org-element-property :parent link)
                   (error nil)))
         (youtube-id (and (member type '("http" "https"))
                          (org-astro--youtube-id-from-url raw-link))))
    (cond
     ;; org-roam ID links → resolve to absolute routes or relative Markdown links
     ((string= type "id")
      (let* ((target-id path)
             (entry (and org-astro--id-path-map
                         (gethash target-id org-astro--id-path-map)))
             (_ (unless entry
                  (message "[ox-astro DEBUG] ID lookup failed: %s (map has %d entries, map=%s)"
                           target-id
                           (if org-astro--id-path-map (hash-table-count org-astro--id-path-map) 0)
                           (if org-astro--id-path-map "exists" "NIL"))))
             (link-text (if (and desc (not (string-blank-p desc)))
                            desc
                            (or (and entry (plist-get entry :title))
                                target-id))))
        (if entry
            ;; Check if we should use absolute routes or relative MDX paths
            ;; Priority: dynamic var (from dest config) > global var
            (let ((effective-base-path (or org-astro--current-id-link-base-path
                                           (and (boundp 'org-astro-id-link-base-path)
                                                org-astro-id-link-base-path))))
              (if effective-base-path
                  ;; Absolute route mode: /base-path/collection-id
                  (let ((collection-id (or (org-astro--compute-collection-id entry)
                                           (org-astro--compute-collection-id-from-outfile entry))))
                    (if collection-id
                        (let* ((base (string-trim-right effective-base-path "/"))
                               (route (concat base "/" collection-id)))
                          (format "[%s](%s)" link-text route))
                        (org-astro--record-missing-id-link info target-id link-text)))
                  ;; Relative MDX path mode (original behavior)
                  (if org-astro--current-outfile
                      (let* ((target-outfile (or (plist-get entry :outfile)
                                                 (let* ((posts-folder (plist-get entry :posts-folder))
                                                        (filename (plist-get entry :filename))
                                                        (relative-dir (plist-get entry :relative-subdir)))
                                                   (when (and posts-folder filename)
                                                     (let ((base (if (and relative-dir (not (string-blank-p relative-dir)))
                                                                     (expand-file-name relative-dir posts-folder)
                                                                     posts-folder)))
                                                       (expand-file-name filename base)))))))
                        (if target-outfile
                            (let ((relative (org-astro--calculate-relative-mdx-path
                                             org-astro--current-outfile target-outfile)))
                              (if relative
                                  (format "[%s](%s)" link-text relative)
                                  (org-astro--record-missing-id-link info target-id link-text)))
                            (org-astro--record-missing-id-link info target-id link-text)))
                      (org-astro--record-missing-id-link info target-id link-text))))
            (org-astro--record-missing-id-link info target-id link-text))))
     ;; Local file image links → render Image component using imports
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (or (string-match-p "\\.\\(png\\|jpe?g\\|webp\\|gif\\)$" path)
               (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp\\|gif)$" path)))
      (let* ((record (org-astro--lookup-render-record path info))
             (_ (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                  (message "[ox-astro][img] LINK path=%s record=%s"
                           path (and record (plist-get record :var-name))))))
        (if record
            (let* ((var-name (plist-get record :var-name))
                   (default-alt (plist-get record :alt))
                   (alt (or desc default-alt "Image")))
              (if var-name
                  (org-astro--image-component-for-record record info alt path)
                  (plist-get record :jsx)))
            (let ((md (when (fboundp 'org-md-link)
                        (ignore-errors (org-md-link link desc info)))))
              (or md (or (org-element-property :raw-link link)
                         (concat (or type "file") ":" path)))))))
     ;; Audio file links → return empty string (import handled by image manifest)
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (string-match-p "\\.\\(mp3\\|wav\\|ogg\\|m4a\\|aac\\|flac\\)$" path))
      "")
     ;; PDF links → emit a Markdown link with URL-encoded spaces; normalize label.
     ((and path
           (or (string= type "file") (and (null type) (string-prefix-p "/" path)))
           (string-match-p "\\.pdf\\(\\?\\|#\\|$\\)" path))
     (let* (;; If this is an absolute local file path under a public/pdfs directory,
             ;; rewrite it to a site path beginning with /pdfs/.
             (site-path
              (cond
               ;; Already a site path
               ((string-prefix-p "/pdfs/" path)
                path)
               ;; Local absolute file path: try to detect /public/pdfs/ segment
               ((and (string-prefix-p "/" path)
                     (string-match "/public/pdfs/\\(.*\\)$" path))
                (concat "/pdfs/" (match-string 1 path)))
               (t
                path)))
             (encoded (if (string-match-p " " site-path)
                          (replace-regexp-in-string " " "%20" site-path)
                          site-path))
             (label-raw (or desc (file-name-base path)))
             ;; Drop any leading symbols like ▌ and normalize "PDF:" spacing
             (label-1 (replace-regexp-in-string "^\\s-*[▌│•▪️]+" "" label-raw))
             (label-a (replace-regexp-in-string "PDF:" "PDF: " label-1))
             (label   (replace-regexp-in-string "PDF:  +" "PDF: " label-a)))
        (format "[%s](%s)" label encoded)))

     ;; Non-image file links (e.g., file+emacs) → regular Markdown link
     ((member type '("file+emacs" "file+sys"))
      (let* ((raw (or (org-element-property :raw-link link) path ""))
             (url (cond
                    ((string-prefix-p "file+emacs:" raw)
                     (concat "file://" (string-remove-prefix "file+emacs:" raw)))
                    ((string-prefix-p "file+sys:" raw)
                     (concat "file://" (string-remove-prefix "file+sys:" raw)))
                    ((string-prefix-p "file:" raw)
                     (concat "file://" (string-remove-prefix "file:" raw)))
                    (t raw)))
             (label (or desc (file-name-nondirectory path) url)))
        (format "[%s](%s)" label url)))

     ;; If the description is already a Markdown link, preserve it unchanged.
     ((and desc (org-astro--contains-markdown-link-p desc))
      desc)
     ;; Internal target (fuzzy) → wiki link or local anchor
     ;; If the target resolves to a local headline, make an anchor link.
     ;; If it doesn't resolve (external wiki link), output [[slug]] format
     ;; for the remarkWikiLink plugin to process.
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((target (condition-case _
                         (and (plist-get info :parse-tree)
                              (org-export-resolve-fuzzy-link link info))
                       (error nil)))
             (resolved-title
              (and target
                   (let ((raw (org-element-property :raw-value target)))
                     (cond
                      ((stringp raw) raw)
                      (t (ignore-errors
                           (org-element-interpret-data
                            (org-element-property :title target)))))))))
        (if target
            ;; Local headline found → anchor link
            ;; Use resolved-title for the anchor (matches what Astro generates for headings)
            ;; Use desc for visible text (or fall back to resolved-title/path)
            (let* ((anchor-source (or resolved-title path))
                   (slug (org-astro--slugify anchor-source))
                   (text (or desc resolved-title path)))
              (format "[%s](#%s)" text slug))
          ;; No local target → wiki link format for remarkWikiLink plugin
          ;; Output [[slug]] or [[slug][display text]] based on whether desc differs from path
          (if (and desc (not (string= desc path)))
              (format "[[%s][%s]]" path desc)
            (format "[[%s]]" path)))))

     ;; Bare YouTube URLs with no description → responsive embed
     ((and (null desc)
           youtube-id
           (org-astro--paragraph-only-raw-link-p parent raw-link))
      (setf (plist-get info :astro-uses-youtube-embed) t)
      (org-astro--youtube-embed youtube-id))

     ;; Bare URLs with no description → LinkPeek + set import flag
     ((and (null desc) (member type '("http" "https" "ftp" "mailto")))
      ;; If this bare URL is part of a Markdown reference link definition
      ;; like "[1]: https://example.com", preserve it literally.
      (if (and parent (org-astro--markdown-link-definition-paragraph-p parent))
          ;; Return the raw URL so the whole line exports as "[x]: url"
          raw-link
          ;; Otherwise, emit LinkPeek + mark for import
          (progn
            (setf (plist-get info :astro-uses-linkpeek) t)
            (format "<LinkPeek href=\"%s\"></LinkPeek>" raw-link))))

     ;; Everything else → prefer org's MD translator; fall back to plain Markdown
     (t
      (let ((md (when (fboundp 'org-md-link)
                  (ignore-errors (org-md-link link desc info)))))
        (or md
            (let* ((raw raw-link)
                   (text (or desc raw)))
              (format "[%s](%s)" text raw))))))))

(defun org-astro-src-block (src-block _contents _info)
  "Transcode a SRC-BLOCK element into fenced Markdown format.
For user, prompt, quote, poetry, verse, and pullquote blocks, preserve
org-mode syntax literally and convert Org headings to markdown
equivalents.

For coding-agent blocks, supports :collapsible header arg:
  :collapsible open  → collapsible open (starts expanded)
  :collapsible t     → collapsible (starts collapsed)
  :collapsible       → collapsible (starts collapsed)"
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             (params (org-element-property :parameters src-block))
             (param-tokens (when params
                             (split-string params "[[:space:]]+" t)))
             ;; Check for :collapsible with optional value
             (collapsible-idx (cl-position ":collapsible" param-tokens :test #'string=))
             (collapsible-value (when collapsible-idx
                                  (let ((next (nth (1+ collapsible-idx) param-tokens)))
                                    (cond
                                     ((null next) t)
                                     ((string= next "open") 'open)
                                     ((string= next "t") t)
                                     ((string-prefix-p ":" next) t)  ; next param, not a value
                                     (t t)))))
             ;; Also check legacy tokens without colon prefix
             (legacy-tokens (mapcar (lambda (tok) (string-trim-left tok ":")) param-tokens))
             (has-legacy-collapsible (and (string= lang "coding-agent")
                                          (not collapsible-value)
                                          (seq-some (lambda (tok)
                                                      (member tok '("collapsible" "foldable" "folded")))
                                                    legacy-tokens)))
             (legacy-open (and has-legacy-collapsible
                               (member "open" legacy-tokens)))
             ;; Build the fence metadata (default coding-agent to collapsible/open if nothing is specified)
             (has-collapsible (or collapsible-value has-legacy-collapsible (string= lang "coding-agent")))
             (explicit-open (or (eq collapsible-value 'open) legacy-open (string= lang "coding-agent")))
             (folded-meta (when (and (string= lang "coding-agent") has-collapsible)
                            (if explicit-open " collapsible open" " collapsible")))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (or (org-astro--resolve-deferred-string
                        (org-element-property :value src-block))
                       "")))
        ;; Handle pullquote blocks specially - wrap in div with blank lines
        (if (string-equal lang "pullquote")
            (let ((processed-code code))
              ;; Convert org-mode links to Markdown links inside pullquotes
              ;; [[path][description]] → [description](path)
              (setq processed-code (replace-regexp-in-string
                                    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                                    "[\\2](\\1)"
                                    processed-code))
              ;; [[path]] → [path](path)
              (setq processed-code (replace-regexp-in-string
                                    "\\[\\[\\([^]]+\\)\\]\\]"
                                    "[\\1](\\1)"
                                    processed-code))
              ;; Convert org-style *bold* to Markdown **bold**.
              (setq processed-code (replace-regexp-in-string
                                    "\\(^\\|[[:space:][:punct:]]\\)\\*\\([^*\n]+\\)\\*\\([[:space:][:punct:]]\\|$\\)"
                                    "\\1**\\2**\\3"
                                    processed-code))
              ;; Convert org-style /italics/ to Markdown *italics*.
              (setq processed-code (replace-regexp-in-string
                                    "\\(^\\|[[:space:][:punct:]]\\)/\\([^/\n]+\\)/\\([[:space:][:punct:]]\\|$\\)"
                                    "\\1*\\2*\\3"
                                    processed-code))
              ;; Convert org-style _underline_ to HTML <u>underline</u>.
              (setq processed-code (replace-regexp-in-string
                                    "\\(^\\|[[:space:][:punct:]]\\)_\\([^_\n]+\\)_\\([[:space:][:punct:]]\\|$\\)"
                                    "\\1<u>\\2</u>\\3"
                                    processed-code))
              (concat "<div class=\"pullquote\">\n\n"
                      (org-trim processed-code)
                      "\n\n</div>\n"))
            (progn
              ;; For user/prompt/quote blocks, convert org-mode syntax to markdown
              (when (member lang '("user" "prompt" "quote" "poetry" "verse"))
                ;; Convert em dashes
                (when (string-match-p "---" code)
                  (setq code (replace-regexp-in-string "---" "—" code)))
                ;; Convert org headings to markdown headings
                (setq code (replace-regexp-in-string "^\\*\\*\\*\\* \\(.*\\)$" "#### \\1" code))
                (setq code (replace-regexp-in-string "^\\*\\*\\* \\(.*\\)$" "### \\1" code))
                (setq code (replace-regexp-in-string "^\\*\\* \\(.*\\)$" "## \\1" code))
                (setq code (replace-regexp-in-string "^\\* \\(.*\\)$" "# \\1" code))
                ;; Convert org-mode links to Markdown links
                ;; [[path][description]] → [description](path)
                (setq code (replace-regexp-in-string
                            "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                            "[\\2](\\1)"
                            code))
                ;; [[path]] → [path](path)
                (setq code (replace-regexp-in-string
                            "\\[\\[\\([^]]+\\)\\]\\]"
                            "[\\1](\\1)"
                            code)))
              ;; Trim trailing newlines/whitespace to prevent extra space at the end.
              (setq code (string-trim-right code))
              (format "```%s%s\n%s\n```" (or lang "") (or folded-meta "") code))))
      ;; Fallback to simple fenced code block
      (let* ((lang (org-element-property :language src-block))
             (params (org-element-property :parameters src-block))
             (param-tokens (when params
                             (split-string params "[[:space:]]+" t)))
             ;; Check for :collapsible with optional value
             (collapsible-idx (cl-position ":collapsible" param-tokens :test #'string=))
             (collapsible-value (when collapsible-idx
                                  (let ((next (nth (1+ collapsible-idx) param-tokens)))
                                    (cond
                                     ((null next) t)
                                     ((string= next "open") 'open)
                                     ((string= next "t") t)
                                     ((string-prefix-p ":" next) t)
                                     (t t)))))
             ;; Also check legacy tokens
             (legacy-tokens (mapcar (lambda (tok) (string-trim-left tok ":")) param-tokens))
             (has-legacy-collapsible (and (string= lang "coding-agent")
                                          (not collapsible-value)
                                          (seq-some (lambda (tok)
                                                      (member tok '("collapsible" "foldable" "folded")))
                                                    legacy-tokens)))
             (legacy-open (and has-legacy-collapsible
                               (member "open" legacy-tokens)))
             (has-collapsible (or collapsible-value has-legacy-collapsible (string= lang "coding-agent")))
             (explicit-open (or (eq collapsible-value 'open) legacy-open (string= lang "coding-agent")))
             (folded-meta (when (and (string= lang "coding-agent") has-collapsible)
                            (if explicit-open " collapsible open" " collapsible")))
             (code (or (org-astro--resolve-deferred-string
                        (org-element-property :value src-block))
                       "")))
        (format "```%s%s\n%s\n```" (or lang "") (or folded-meta "") (org-trim code)))))

(defun org-astro-example-block (example-block _contents _info)
  "Transcode an EXAMPLE-BLOCK element into fenced Markdown format.
If the block has switches (e.g., #+begin_example fountain collapsible),
use the first switch as the language and remaining switches as meta.
Otherwise, output as a plain fenced code block."
  (let* ((switches (org-element-property :switches example-block))
         (tokens (when switches
                   (seq-filter
                    (lambda (token) (not (string-prefix-p "-" token)))
                    (split-string switches "[[:space:]]+" t))))
         (lang (car tokens))
         (meta (when (cdr tokens)
                 (concat " " (string-join (cdr tokens) " "))))
         (code (or (org-astro--resolve-deferred-string
                    (org-element-property :value example-block))
                   "")))
    (format "```%s%s\n%s\n```" (or lang "") (or meta "") (org-trim code))))

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
               (header (concat (make-string level ?#) " " title))
               (theme-prefix (org-astro--theme-prefix-for-heading heading info))
               (model-prefix (org-astro--model-prefix-for-heading heading info))
               (prefix (concat (or theme-prefix "") (or model-prefix ""))))
          ;; If we hoisted a theme marker, drop the original from contents to avoid duplication.
          (when (and theme-prefix contents (string-prefix-p theme-prefix contents))
            (setq contents (substring contents (length theme-prefix))))
          ;; If we hoisted a model banner, drop the original from contents to avoid duplication.
          (when (and model-prefix contents (string-prefix-p model-prefix contents))
            (setq contents (substring contents (length model-prefix))))
          (concat prefix header "\n\n" (or contents ""))))))

(defun org-astro--theme-prefix-for-heading (heading _info)
  "Emit a theme marker if HEADING is immediately followed by a THEME keyword.
Marks the keyword as consumed so it won't render twice."
  (let* ((section (car (org-element-contents heading)))
         (first-child (when (and section (eq 'section (org-element-type section)))
                        (car (org-element-contents section)))))
    (when (and first-child
               (eq (org-element-type first-child) 'keyword)
               (string-equal (org-element-property :key first-child) "THEME"))
      (let* ((raw (org-element-property :value first-child))
             (value (and raw (string-trim raw))))
        (when (and value (not (string-empty-p value)))
          (org-element-put-property first-child :astro-consumed t)
          (format "{/* theme: %s */}\n\n" (downcase value)))))))

(defun org-astro--model-prefix-for-heading (heading _info)
  "Emit a model banner if HEADING section starts with a MODEL keyword.
Allows THEME to appear first; marks MODEL keyword as consumed."
  (let* ((section (car (org-element-contents heading)))
         (children (when (and section (eq 'section (org-element-type section)))
                     (org-element-contents section))))
    (when children
      (cl-labels ((blank-paragraph-p (node)
                    (and (eq (org-element-type node) 'paragraph)
                         (string-blank-p (org-element-interpret-data node))))
                  (next-meaningful (nodes)
                    (cl-loop for node in nodes
                             unless (blank-paragraph-p node)
                             return node)))
        (let* ((first (next-meaningful children))
               (rest (and first (cdr (member first children))))
               (second (and rest (next-meaningful rest)))
               (model-node
                (cond
                 ((and first
                       (eq (org-element-type first) 'keyword)
                       (string-equal (org-element-property :key first) "MODEL"))
                  first)
                 ((and first second
                       (eq (org-element-type first) 'keyword)
                       (string-equal (org-element-property :key first) "THEME")
                       (eq (org-element-type second) 'keyword)
                       (string-equal (org-element-property :key second) "MODEL"))
                  second))))
          (when model-node
            (let* ((raw (org-element-property :value model-node))
                   (value (and raw (string-trim raw))))
              (when (and value (not (string-empty-p value)))
                (org-element-put-property model-node :astro-consumed t)
                (format "<div class=\"model-banner\">%s</div>\n\n" value)))))))))

(defun org-astro--handle-broken-image-paragraph (paragraph info)
  "Handle a paragraph containing a broken image path with subscripts."
  (let* ((path (org-astro--extract-image-path-from-paragraph paragraph))
         (record (and path (org-astro--lookup-render-record path info))))
    (if record
        (org-astro--image-component-for-record record info nil path)
        "")))


(defun org-astro--add-soft-breaks (text)
  "Add Markdown soft breaks (two trailing spaces) before internal newlines in TEXT.
This preserves single line breaks in the rendered output."
  (when text
    (let* ((lines (split-string text "\n"))
           (num-lines (length lines)))
      (mapconcat
       'identity
       (cl-loop for line in lines
                for i from 1
                collect (if (and (< i num-lines)
                                 (not (string-empty-p (string-trim line))))
                            (concat (string-trim-right line) "  ")
                          line))
       "\n"))))


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
        (let* ((record (org-astro--lookup-render-record path info)))
          (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
            (message "[ox-astro][img] PARA path=%s record=%s"
                     path (and record (plist-get record :var-name))))
          (if record
              (org-astro--image-component-for-record record info nil path)
              contents))
        ;; Check if this paragraph contains broken image path (subscripts)
        (let ((paragraph-context (org-element-interpret-data paragraph)))
          (if (string-match-p "/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)" paragraph-context)
              ;; This paragraph contains a broken image path - try to handle it
              (org-astro--handle-broken-image-paragraph paragraph info)
              ;; Regular paragraph - add soft breaks for single newlines
              (org-astro--add-soft-breaks
               (org-md-paragraph paragraph contents info)))))))


(defun org-astro-plain-text (text info)
  "Transcode a plain-text element.
If the text contains raw image paths on their own lines, convert them to
<img> tags. If the text contains raw URLs on their own lines, convert them
to LinkPeek components."
  (let* ((lines (split-string text "\n"))
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line)))
               (if (org-astro--contains-markdown-link-p line)
                   ;; Preserve Markdown link formatting unchanged
                   line
                   (cond
                    ;; Raw image path (trust precomputed render map)
                    ((and trimmed-line
                          (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" trimmed-line)
                              (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" trimmed-line)))
                     (let ((record (org-astro--lookup-render-record trimmed-line info)))
                       (if record
                           (org-astro--image-component-for-record record info nil trimmed-line)
                           line)))
                    ;; Remote image URL
                    ((and trimmed-line
                          (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" trimmed-line))
                     (let ((record (org-astro--lookup-render-record trimmed-line info)))
                       (if record
                           (org-astro--image-component-for-record record info nil trimmed-line)
                           line)))
                    ;; Regular remote URL (non-image) is now handled correctly by org-astro-link.
                    ;; Regular line
                    (t line)))))
           lines)))
    ;; Store LinkPeek usage in info for import generation
    (when has-linkpeek
      (plist-put info :astro-uses-linkpeek t))
    (mapconcat 'identity processed-lines "\n")))


(defun org-astro-subscript (subscript contents _info)
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



;;;; Export Block Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-export-block (export-block _contents info)
  "Transcode EXPORT-BLOCK element into MDX format.
Recognizes MDX, MARKDOWN, and MD export blocks and passes their content
through verbatim."
  (let ((type (org-element-property :type export-block)))
    (if (member (upcase type) '("MDX" "MARKDOWN" "MD"))
        ;; Pass through the content completely unindented to prevent the final-output
        ;; filter from converting indented lines to blockquotes
        (let ((content (org-element-property :value export-block)))
          (replace-regexp-in-string "^[ \t]+" "" content))
        ;; For other types, fall back to HTML backend
        (org-export-with-backend 'html export-block nil info))))

(provide 'ox-astro-transcode)
;;; ox-astro-transcode.el ends here
