;;; ------------------------------------------------------------------
;;; CUT HERE FOR ox-astro-functions.el
;;;
;;; This file should contain all helper functions (defun).
;;; ------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
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

(defun org-astro--get-front-matter-data (tree info)
  "Build an alist of final front-matter data, applying defaults."
  (let* (;; Get the posts-folder, needed for processing image paths.
         (posts-folder (or (plist-get info :posts-folder)
                           (plist-get info :astro-posts-folder)))
         ;; --- Metadata with defaults (respecting narrowing) ---
         (title
          (or (let ((kw (org-element-map tree 'keyword
                          (lambda (k) (when (string-equal "TITLE" (org-element-property :key k)) k))
                          nil 'first-match)))
                (when kw (org-element-property :value kw)))
              (let ((headline (org-element-map tree 'headline 'identity nil 'first-match)))
                (when headline
                  (org-export-data (org-element-property :title headline) info)))
              "Untitled Post"))
         (author (or (plist-get info :author) "Jay Dixit"))
         (excerpt
          (or (let ((kw (org-element-map tree 'keyword
                          (lambda (k)
                            (when (or (string-equal "ASTRO_EXCERPT" (org-element-property :key k))
                                      (string-equal "EXCERPT" (org-element-property :key k)))
                              k))
                          nil 'first-match)))
                (when kw (replace-regexp-in-string "[*]" "" (org-element-property :value kw))))
              (let ((paragraph (org-element-map tree 'paragraph
                                 'org-element-contents
                                 nil 'first-match)))
                (when paragraph
                  (replace-regexp-in-string "[*]" "" (org-export-data paragraph info))))
              ""))
         (tags-raw (or (plist-get info :astro-tags) (plist-get info :tags)))
         (tags (when tags-raw (org-split-string tags-raw "[, \n]+")))
         ;; --- Publish Date (with fallback to current time) ---
         (publish-date
          (let ((date-raw (or (plist-get info :astro-publish-date)
                              (plist-get info :publish-date)
                              (plist-get info :date))))
            (if date-raw
                (org-astro--format-date date-raw info)
                (format-time-string (plist-get info :astro-date-format) (current-time)))))
         ;; --- Author Image (with default and specific path) ---
         (author-image-raw (or (plist-get info :astro-author-image)
                               (plist-get info :author-image)
                               org-astro-default-author-image))
         (author-image (and author-image-raw posts-folder
                            (org-astro--process-image-path
                             author-image-raw posts-folder "authors/")))
         ;; --- Cover Image & Alt Text (with generated alt text) ---
         (image-raw (or (plist-get info :astro-image)
                        (plist-get info :cover-image)))
         (image (and image-raw posts-folder
                     (org-astro--process-image-path
                      image-raw posts-folder "posts/")))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and image (org-astro--filename-to-alt-text image)))))
    ;; Return the alist of final data
    `((title . ,title)
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      (tags . ,tags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX.
This handles raw URLs specially to format them as [url](url)
instead of <url>."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (raw-link (org-element-property :raw-link link)))
    (cond
     ;; Fuzzy links for internal headings
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((target (org-export-resolve-fuzzy-link link info))
             (title (org-element-property :raw-value target))
             (slug (org-astro--slugify title)))
        (concat "[" (or desc title) "](" (string ?#) slug ")")))
     ;; Raw URLs (no description)
     ((and (not desc) (member type '("http" "https" "ftp" "mailto")))
      (format "[%s](%s)" raw-link raw-link))
     ;; Defer to default markdown for all other links
     (t
      (org-md-link link desc info)))))

(defun org-astro-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element into fenced Markdown format."
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        (when (and (member lang '("user" "prompt" "quote")) (string-match-p "---" code))
          (setq code (replace-regexp-in-string "---" "—" code)))
        ;; Remove any trailing newlines to prevent extra space at the end.
        (setq code (replace-regexp-in-string "\\`\n+\\|\\s-+\\'" "" code))
        (format "```%s\n%s\n```" (or lang "") code))
      (org-html-textarea-block src-block contents info)))

(defun org-astro-heading (heading contents info)

  "Transcode a HEADING element.
If it has a TODO keyword, convert it to a Markdown task list item."
  (let ((todo-keyword (org-element-property :todo-keyword heading)))
    (if todo-keyword
        ;; It's a TODO item, format as a task list.
        (let* ((title (org-export-data (org-element-property :title heading)
                                       (cons :with-smart-quotes (cons nil info))))
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
        ;; It's a regular heading.
        (let* ((title (org-export-data (org-element-property :title heading)
                                       (cons :with-smart-quotes (cons nil info))))
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
                   (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" text)
                   (file-exists-p text))
          (setq is-image-path t)
          (setq path text))))

    (if is-image-path
        (let* ((image-imports (plist-get info :astro-body-images-imports))
               (image-data (cl-find path image-imports
                                    :key (lambda (item) (plist-get item :path))
                                    :test #'string-equal)))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
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
             (let ((trimmed-line (org-trim line)))
               (cond
                ;; Raw image path
                ((and trimmed-line
                      (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" trimmed-line)
                      (file-exists-p trimmed-line))
                 (let ((image-data (cl-find trimmed-line image-imports
                                            :key (lambda (item) (plist-get item :path))
                                            :test #'string-equal)))
                   (if image-data
                       (let ((var-name (plist-get image-data :var-name))
                             (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                         (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                       ;; Fallback: if image wasn't processed, return empty string to remove the raw path
                       "")))
                ;; Raw URL
                ((and trimmed-line
                      (string-match-p "^https?://[^[:space:]]+$" trimmed-line))
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
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-prepare-images-filter (tree _backend info)
  "Find all local images, process them, and store import data in INFO.
This filter runs on the parse TREE before transcoding. It collects
all local image links, copies them to the Astro assets
directory, and prepares a list of import statements to be added
to the final MDX file. The data is stored in the INFO plist
under the key `:astro-body-images-imports`."
  (let* ((posts-folder (or (plist-get info :posts-folder)
                           (plist-get info :astro-posts-folder)))
         ;; Collect all image links from the document body.
         (image-paths (org-astro--collect-images-from-tree tree))
         image-imports-data)
    (when posts-folder
      (dolist (path image-paths)
        ;; For each image, copy it to assets and get its new path.
        (let* ((astro-path (org-astro--process-image-path path posts-folder "posts/"))
               (var-name (org-astro--path-to-var-name path)))
          (when (and astro-path var-name)
            (push `(:path ,path :var-name ,var-name :astro-path ,astro-path)
                  image-imports-data)))))
    ;; Store the collected data in the info plist for other functions to use.
    (when image-imports-data
      (let ((final-data (nreverse image-imports-data)))
        (plist-put info :astro-body-images-imports final-data))))
  ;; Return the tree, as required for a parse-tree filter.
  tree)

(defun org-astro-body-filter (body _backend info)
  "Add front-matter and imports to the BODY of the document."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--get-front-matter-data tree info))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (body-images-imports (plist-get info :astro-body-images-imports))
         (body-imports-string
          (when body-images-imports
            (mapconcat
             (lambda (item)
               (format "import %s from '%s';"
                       (plist-get item :var-name)
                       (plist-get item :astro-path)))
             body-images-imports
             "\n")))
         ;; 2. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 3. Astro Image component import (always include if we have any body images)
         (astro-image-import (when body-images-imports
                               "import { Image } from 'astro:assets';"))
         ;; 4. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (or (plist-get info :astro-uses-linkpeek)
                                    (string-match-p "\\[\\(https?://[^]]+\\)\\](\\1)" body))
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list astro-image-import linkpeek-import body-imports-string manual-imports))
                                 "\n")))
    (concat front-matter-string
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
         ;; HTML entities
         (s (replace-regexp-in-string "&#x2013;" "–" s t t))
         (s (replace-regexp-in-string "&rsquo;" "'" s t t))
         (s (replace-regexp-in-string "&lsquo;" "'" s t t))
         (s (replace-regexp-in-string "&rdquo;" "\"" s t t))
         (s (replace-regexp-in-string "&ldquo;" "\"" s t t))
         ;; Convert markdown image syntax with absolute paths to Image components
         (image-imports (plist-get info :astro-body-images-imports))
         (s (if image-imports
                (replace-regexp-in-string
                 "!\\[\\([^]]*\\)\\](\\(/[^)]+\\.\\(?:png\\|jpe?g\\)\\))"
                 (lambda (match)
                   (let* ((alt (match-string 1 match))
                          (path (match-string 2 match))
                          (image-data (cl-find path image-imports
                                               :key (lambda (item) (plist-get item :path))
                                               :test #'string-equal)))
                     (if image-data
                         (let ((var-name (plist-get image-data :var-name))
                               (alt-text (or (org-astro--filename-to-alt-text path) alt "Image")))
                           (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                         match)))
                 s)
                s))
         ;; Convert markdown links that are raw URLs to LinkPeek components
         (s (replace-regexp-in-string
             "\\[\\(https?://[^]]+\\)\\](\\(\\1\\))"
             (lambda (match)
               (let ((url (match-string 1 match)))
                 ;; Mark that we're using LinkPeek (for import)
                 (plist-put info :astro-uses-linkpeek t)
                 (format "<LinkPeek href=\"%s\"></LinkPeek>" url)))
             s))
         ;; Indented blocks to blockquotes
         (lines (split-string s "\n"))
         (processed-lines
          (mapcar (lambda (line)
                    (if (string-prefix-p "    " line)
                        (concat "> " (substring line 4))
                        line))
                  lines))
         (s (mapconcat 'identity processed-lines "\n")))
    s))

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
                         (string-match-p "^/.*\\.\\(png\\|jpe?g\\)$" text)
                         (file-exists-p text))
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
                       (directory-file-name posts-dir))))))
      (expand-file-name (concat "assets/images/" sub-dir) src-dir))))

(defun org-astro--process-image-path (image-path posts-folder sub-dir)
  "Process IMAGE-PATH, copying to SUB-DIR and returning relative path."
  (when (and image-path posts-folder)
    (let* ((clean-path (replace-regexp-in-string
                        "['\"]" "" (org-trim image-path)))
           (expanded-path (expand-file-name clean-path))
           (assets-folder (org-astro--get-assets-folder posts-folder sub-dir)))
      (if (and (file-exists-p expanded-path) assets-folder)
          (let* ((filename (file-name-nondirectory expanded-path))
                 (dest-path (expand-file-name filename assets-folder)))
            (make-directory assets-folder t)
            (unless (file-exists-p dest-path)
              (message "Copying %s to %s" expanded-path dest-path)
              (copy-file expanded-path dest-path t))
            ;; Return the path for Astro's import syntax
            (format "~/assets/images/%s%s" sub-dir filename))
          image-path))))

(defun org-astro--insert-keyword-at-end-of-block (key value)
  "Insert #+KEY: VALUE at the end of the Org keyword block."
  (save-excursion
    (goto-char (point-min))
    (let ((insert-point (point-min))
          (found-keywords nil))
      ;; Find the last line that is a keyword
      (while (re-search-forward "^#\\+[A-Z_]+:" nil t)
        (setq found-keywords t)
        (setq insert-point (point-at-eol)))
      (goto-char insert-point)
      ;; If we found keywords, insert after them. Otherwise, at the top.
      (if found-keywords (end-of-line))
      (insert (format "\n#+%s: %s" (upcase key) value)))))


(provide 'ox-astro-functions)

;;; ------------------------------------------------------------------
;;; END CUT FOR ox-astro-functions.el
;;;
;;; ------------------------------------------------------------------
