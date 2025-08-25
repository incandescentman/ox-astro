;;; ox-astro-helpers.el --- Helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:

(require 'subr-x) ; for string-trim, string-trim-right

;; Declare global variable for data persistence across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

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
                ;; In full file: place at top after properties/keywords
                (progn
                  ;; First, skip over any org-roam properties block
                  (when (looking-at-p "^:PROPERTIES:")
                    (forward-line 1)
                    (while (and (< (point) limit)
                                (not (looking-at-p "^:END:")))
                      (forward-line 1))
                    (when (looking-at-p "^:END:")
                      (forward-line 1)))
                  ;; Then skip over keywords, comments, and blank lines
                  (while (and (< (point) limit)
                              (or (looking-at-p "^#\\+")
                                  (looking-at-p "^#\\s-")
                                  (looking-at-p "^\\s-*$")))
                    (forward-line 1))))
            ;; Keep one blank line before insert unless at BOF or already blank
            (unless (or (bobp) (looking-at-p "^\\s-*$"))
              (insert "\n"))
            (insert (format "#+%s: %s\n" ukey (or value ""))))))))

;; Back-compat alias for existing calls
(defun org-astro--insert-keyword-at-end-of-block (key value)
  "Alias for `org-astro--upsert-keyword'."
  (org-astro--upsert-keyword key value))


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
           (parts (split-string clean-filename "[-]"))
           (var-name (if (null parts)
                         ""
                       (concat (car parts)
                               (mapconcat #'capitalize (cdr parts) "")))))
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
              (one   (replace-regexp-in-string "\n" " " clean)))
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
  "Get the cover image path and alt text from INFO."
  (let* ((image-raw (or (plist-get info :astro-image)
                        (plist-get info :cover-image)))
         (image (and image-raw posts-folder
                     (org-astro--process-image-path image-raw posts-folder "posts/")))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and image (org-astro--filename-to-alt-text image)))))
    (list image image-alt)))

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
         (publish-date (org-astro--get-publish-date info))
         (author-image (org-astro--get-author-image info posts-folder))
         (cover-image-data (org-astro--get-cover-image info posts-folder))
         (image (car cover-image-data))
         (image-alt (cadr cover-image-data))
         (visibility (plist-get info :visibility))
         (hidden (when (and visibility (string= (downcase (org-trim visibility)) "hidden")) t))
         (status (plist-get info :status))
         (draft (when (and status (string= (downcase (org-trim status)) "draft")) t)))
    ;; Return the alist of final data - only include hidden/draft if they're true
    `((title . ,title)
      ,@(when slug `((slug . ,slug)))
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      (tags . ,tags)
      ,@(when hidden `((hidden . ,hidden)))
      ,@(when draft `((draft . ,draft))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX, robust to partial INFO during preflight."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link)))
    (cond
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
  (let* ((image-imports (or (plist-get info :astro-body-images-imports)
                            org-astro--current-body-images-imports))
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
  (let* ((children (org-element-contents paragraph))
         (child (and (= 1 (length children)) (car children)))
         (is-image-path nil)
         path)
    (when (and child (eq 'plain-text (org-element-type child)))
      (let* ((raw-text (org-element-property :value child))
             (text (when (stringp raw-text) (org-trim raw-text))))
        (when (and text
                   (string-match-p "^/.*\\.\(png\\|jpe?g\\|webp\)$" text)
                   (file-exists-p text))
          (setq is-image-path t)
          (setq path text))))
    (if is-image-path
        (let* ((image-imports (or (plist-get info :astro-body-images-imports)
                                  org-astro--current-body-images-imports))
               (image-data (when image-imports
                             (cl-find path image-imports
                                      :key (lambda (item) (plist-get item :path))
                                      :test #'string-equal))))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\" %s \" />" var-name alt-text))
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
                    ;; Raw image path
                    ((and trimmed-line
                          (string-match-p "^/.*\\.\(png\\|jpe?g\\|webp\)$" trimmed-line)
                          (file-exists-p trimmed-line))
                     (let ((image-data (when image-imports
                                         (cl-find trimmed-line image-imports
                                                  :key (lambda (item) (plist-get item :path))
                                                  :test #'string-equal))))
                       (if image-data
                           (let ((var-name (plist-get image-data :var-name))
                                 (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                             (format "<Image src={%s} alt=\" %s \" />" var-name alt-text))
                           ;; Fallback: if image wasn't processed, return empty string to remove the raw path
                           "")))
                    ;; Raw URL
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
                         (string-match-p "^/.*\\.\(png\\|jpe?g\\|webp\)$" text)
                         (file-exists-p text))
                (push text images)))))))
    ;; 3. Collect from paragraphs that contain subscript elements (broken up image paths)
    (org-element-map tree 'paragraph
      (lambda (paragraph)
        (let ((reconstructed-path (org-astro--extract-image-path-from-paragraph paragraph)))
          (when (and reconstructed-path
                     (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" reconstructed-path)
                     (file-exists-p reconstructed-path))
            (push reconstructed-path images)))))
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
          (when (file-exists-p path)
            (push path images)))))
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

(defun org-astro--process-image-path (image-path posts-folder sub-dir)
  "Process IMAGE-PATH for POSTS-FOLDER, copying to SUB-DIR if needed.
Returns the processed path suitable for Astro imports."
  (when (and image-path posts-folder)
    (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
           (original-filename (file-name-nondirectory image-path))
           (clean-filename (org-astro--sanitize-filename original-filename))
           (target-path (when assets-folder (expand-file-name clean-filename assets-folder))))
      (when (and target-path (file-exists-p image-path))
        ;; Create assets directory if it doesn't exist
        (when assets-folder
          (make-directory assets-folder t))
        ;; Copy the file
        (condition-case err
            (copy-file image-path target-path t)
          (error (message "Failed to copy image %s: %s" image-path err)))
        ;; Return the alias path for imports
        (when (file-exists-p target-path)
          (concat "~/assets/images/" sub-dir clean-filename))))))

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

(provide 'ox-astro-helpers)

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
            (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\(png\|jpe?g\|webp\|jpg\)\s-*$" nil t)
              (let ((path (string-trim (match-string 0))))
                (when (file-exists-p path)
                  (push path images))))))))
    (nreverse images)))
;;; ox-astro-helpers.el ends here

