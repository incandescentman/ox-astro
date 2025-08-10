;;; ox-astro-helpers.el --- Helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:

(defun org-astro--safe-export (data info)
  "Like `org-export-data' but never throws. Falls back to readable plain text."
  (condition-case _
      (org-trim (org-export-data data info))
    (error
     (let* ((s (org-no-properties (org-element-interpret-data data))))
       ;; [[url][desc]] -> desc, [[url]] -> url
       (setq s (replace-regexp-in-string "\[\[\([^]]+\)\]\[\([^]]+\)\]\]" "\2" s))
       (setq s (replace-regexp-in-string "\[\[\([^]]+\)\]\]" "\1" s))
       ;; Remove any remaining markdown formatting
       (setq s (replace-regexp-in-string "[*_]" "" s))
       ;; Remove any remaining Org-specific formatting
       (setq s (replace-regexp-in-string "\\\(\|\\\[\|\\\\\]\)" "" s))
       (org-trim s)))))

(defun org-astro--slugify (s)
  "Convert string S to a slug."
  (when (stringp s)
    (let ((s (downcase s)))
      (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil))))

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
           ;; Remove excessive numbers/dates and clean up
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9] [0-9][0-9] [0-9][0-9] " " " human-readable))
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9][0-9]+ " " " cleaned))
           (cleaned (org-trim cleaned)))
      (capitalize cleaned))))

(defun org-astro--path-to-var-name (path)
  "Convert a file PATH to a camelCase JS variable name."
  (when (stringp path)
    (let* ((filename (file-name-sans-extension (file-name-nondirectory path)))
           (parts (split-string filename "[-_]")))
      (if (null parts)
          ""
          (concat (car parts)
                  (mapconcat #'capitalize (cdr parts) ""))))))

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
        (dolist (pair data))
          (let ((key (car pair))
                (val (cdr pair))))
            (when val
              (setq yaml-str
                    (concat yaml-str
                            (format "%s: " (symbol-name key))
                            (cond
                             ((listp val)
                              (concat "\n"
                                      (mapconcat
                                       (lambda (item) (format "- %s" item))
                                       val "\n")
                                      "\n"))
                             (t (format "%s\n"
                                        (if (and (stringp val)
                                                 (string-match-p ":" val)
                                                 (not (eq key 'publishDate)))
                                            (format "\"%s\"" (replace-regexp-in-string "\"" "\\\"" val))
                                            val)))))))))
        (concat yaml-str "---\n"))))

(defun org-astro--get-title (tree info)
  "Extract post title from TREE, with fallbacks.
It checks for a #+TITLE keyword, then the first headline,
and finally defaults to 'Untitled Post'."
  (or (let ((kw (org-element-map tree 'keyword
                  (lambda (k) (when (string-equal "TITLE" (org-element-property :key k)) k))
                  nil 'first-match)))
        (when kw (org-element-property :value kw)))
      (let ((headline (org-element-map tree 'headline 'identity nil 'first-match)))
        (when headline
          (org-export-data (org-element-property :title headline) info)))
      "Untitled Post"))

(defun org-astro--get-excerpt (tree info)
  "Extract post excerpt from TREE, with fallbacks.
It checks for #+ASTRO_EXCERPT or #+EXCERPT, then the first
paragraph, and finally defaults to an empty string."
  (or (let ((kw (org-element-map tree 'keyword
                          (lambda (k)
                            (when (or (string-equal "ASTRO_EXCERPT" (org-element-property :key k))
                                      (string-equal "EXCERPT" (org-element-property :key k)))
                              k))
                          nil 'first-match)))
        (when kw (replace-regexp-in-string "[*_/]" "" (org-element-property :value kw))))
      (let ((paragraph (org-element-map tree 'paragraph
                                 'org-element-contents
                                 nil 'first-match)))
        (when paragraph
          (replace-regexp-in-string "[*_/]" "" (org-export-data paragraph info))))
      ""))

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
                     (org-astro--process-image-path
                      image-raw posts-folder "posts/")))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and image (org-astro--filename-to-alt-text image)))))
    (list image image-alt)))

(defun org-astro--get-front-matter-data (tree info)
  "Build an alist of final front-matter data, applying defaults."
  (let* ((posts-folder (or (plist-get info :destination-folder)
                           (plist-get info :astro-posts-folder)))
         (title (org-astro--get-title tree info))
         (author (or (plist-get info :author) "Jay Dixit"))
         (excerpt (org-astro--get-excerpt tree info))
         (tags-raw (or (plist-get info :astro-tags) (plist-get info :tags)))
         (tags (when tags-raw (org-split-string tags-raw "[, \n]+\n")))
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
  "Transcode a LINK object for Astro MDX.
This handles raw URLs specially to format them as <LinkPeek> components."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (cond
     ;; Fuzzy links for internal headings (be resilient if INFO is incomplete)
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((resolved-title
              (condition-case _
                  (let* ((target (and (plist-get info :parse-tree)
                                      (org-export-resolve-fuzzy-link link info))))
                    (and target (org-element-property :raw-value target)))
                (error nil)))
             (title (or desc resolved-title path))
             (slug  (org-astro--slugify title)))
        (format "[[#%s][%s]]" (or desc title) slug)))

     ;; Raw URLs (no description) - note: plist-put return value is ignored, so this was a no-op.
     ;; Optional improvement (not required for the crash): detect <LinkPeek> later in the body filter.
     ((and (null desc) (member type '("http" "https" "ftp" "mailto")))
      (format "<LinkPeek href=\"%s\"></LinkPeek>" path))

     ;; Everything else → default Markdown
     (t
      (org-md-link link desc info)))))


(defun org-astro-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element into fenced Markdown format."
  (if (not (org-export-read-attribute :attr_md src-block :textarea)))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        (when (and (member lang '("user" "prompt" "quote")) (string-match-p "---" code)))
          (setq code (replace-regexp-in-string "---" "—" code)))
        ;; Remove any trailing newlines to prevent extra space at the end.
        (setq code (replace-regexp-in-string "\\`\n+\|\s-+\'" "" code))
        (format "```%s\n%s\n```" (or lang "") code))
      (org-html-textarea-block src-block contents info)))

(defun org-astro-heading (heading contents info)
  (let ((todo-keyword (org-element-property :todo-keyword heading)))
    (if todo-keyword
        ;; task style
        (let* ((title (org-astro--safe-export (org-element-property :title heading)
                                              (plist-put (copy-plist info) :with-smart-quotes nil)))
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
      (let* ((title (org-astro--safe-export (org-element-property :title heading)
                                            (plist-put (copy-plist info) :with-smart-quotes nil)))
             (level (+ (org-element-property :level heading)
                       (or (plist-get info :headline-offset) 0)))
             (header (format "%s %s" (make-string level ?#) title)))
        (format "%s\n\n%s" header (or contents ""))))))

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
                   (string-match-p "^/.*\\.\(png\|jpe?g\)$" text)
                   (file-exists-p text)))
          (setq is-image-path t)
          (setq path text))))

    (if is-image-path
        (let* ((image-imports (plist-get info :astro-body-images-imports))
               (image-data (when image-imports
                             (cl-find path image-imports
                                      :key (lambda (item) (plist-get item :path))
                                      :test #'string-equal))))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\" %s \" />" var-name alt-text))
              ;; Fallback: if image wasn't processed by the filter, just output the path as text.
              contents))
        ;; Not an image path, use default paragraph handling
        (org-md-paragraph paragraph contents info))))

(defun org-astro-plain-text (text info)
  "Transcode a plain-text element.
If the text contains raw image paths on their own lines, convert them to <img> tags.
If the text contains raw URLs on their own lines, convert them to LinkPeek components."
  (let* ((lines (split-string text "\n"))
         (image-imports (plist-get info :astro-body-images-imports))
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line))))
               (cond
                ;; Raw image path
                ((and trimmed-line
                      (string-match-p "^/.*\\.\(png\|jpe?g\)$" trimmed-line)
                      (file-exists-p trimmed-line)))
                 (let ((image-data (when image-imports
                                     (cl-find trimmed-line image-imports
                                              :key (lambda (item) (plist-get item :path))
                                              :test #'string-equal)))))
                   (if image-data
                       (let ((var-name (plist-get image-data :var-name))
                             (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                         (format "<Image src={%s} alt=\" %s \" />" var-name alt-text))
                       ;; Fallback: if image wasn't processed, return empty string to remove the raw path
                       "")))
                ;; Raw URL
                ((and trimmed-line
                      (string-match-p "^https?://[^[:space:]]+$" trimmed-line)))
                 (setq has-linkpeek t)
                 (format "<LinkPeek href=\"%s\"></LinkPeek>" trimmed-line))
                ;; Regular line
                (t line))))
           lines)))
    ;; Store LinkPeek usage in info for import generation
    (when has-linkpeek
      (plist-put info :astro-uses-linkpeek t))
    (mapconcat 'identity processed-lines "\n"))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--collect-images-from-tree (tree)
  "Collect all image paths from the parse TREE.
This includes both `[[file:...]]` links and raw image paths on their own line."
  (let (images)
    ;; 1. Collect from `link` elements
    (org-element-map tree 'link
      (lambda (link))
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link))))
          (when (and (string= type "file")
                     (string-match-p "\\(png\|jpg\|jpeg\|gif\|svg\|webp\)$" path)))
            (push path images)))))
    ;; 2. Collect from raw paths in all plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element))
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n")))))
          (dolist (line lines))
            (let ((text (org-trim line))))
              (when (and text
                         (string-match-p "^/.*\\.\(png\|jpe?g\)$" text)
                         (file-exists-p text)))
                (push text images)))))))
    ;; Return a list with no duplicates
    (delete-dups (nreverse images))))

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
                       (directory-file-name posts-dir)))))))
      (expand-file-name (concat "assets/images/" sub-dir) src-dir))))

(defun org-astro--process-image-path (image-path posts-.el ends here