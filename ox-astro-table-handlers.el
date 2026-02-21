;;; ox-astro-table-handlers.el --- Table handling functions for ox-astro  -*- lexical-binding: t -*-

;;; Commentary:
;; Table handling functions extracted from ox-astro-helpers.el

;;; Code:

(require 'org)
(require 'ox)

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
        (org-astro--sanitize-table-cell
         (org-trim (org-export-data cell-contents info)))
      "")))

(defun org-astro--sanitize-table-cell (text)
  "Normalize TABLE cell TEXT for MDX compatibility.
Ensure self-closing tags like <br> become `<br />` and wrap generic type
signatures (e.g., `map<string>`) in inline code so MDX doesn't parse the
angle brackets as JSX."
  (let* ((raw (or text ""))
         (normalized raw))
    (dolist (needle '("<br>" "<br/>" "<br />" "<br  />"))
      (setq normalized (replace-regexp-in-string needle "<br />" normalized t t)))
    (if (and (not (equal normalized ""))
             (not (string-match-p "`" normalized))
             (string-match-p "\\`[[:alnum:]-_]+<.*>\\'" normalized))
        (concat "`" normalized "`")
      normalized)))

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
             (gallery-items nil)
             ;; Parse :columns attribute from #+BEGIN_GALLERY :columns N
             ;; Special blocks store parameters as a string like ":columns 12"
             (params-string (org-element-property :parameters special-block))
             (columns (when (and params-string (string-match ":columns +\\([0-9]+\\)" params-string))
                        (string-to-number (match-string 1 params-string)))))
        ;; Debug: log what's in image-imports
        (org-astro--dbg-log info "GALLERY: image-imports contains %d items" (length image-imports))
        (when image-imports
          (org-astro--dbg-log info "GALLERY: import paths: %s"
                              (mapcar (lambda (item) (plist-get item :path)) image-imports)))
        ;; First, extract image links from the block contents
        (org-element-map special-block 'link
          (lambda (link)
            (when (and (string-equal (org-element-property :type link) "file")
                       (let ((path (org-element-property :path link)))
                         (and path (string-match-p "\\.\\(png\\|jpe?g\\|webp\\|avif\\)$" path))))
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
                (while (re-search-forward "^\\s-*/[^[:space:]]+\\.\\(png\\|jpe?g\\|webp\\|avif\\)\\s-*$" nil t)
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
        (org-astro--dbg-log info "GALLERY generating component with %d images, columns=%s"
                            (length gallery-items) columns)
        (if gallery-items
            (concat "<ImageGallery\n"
                    "  images={[\n"
                    (mapconcat #'identity (reverse gallery-items) ",\n")
                    "\n  ]}\n"
                    "  galleryId=\"" gallery-id "\"\n"
                    (when (and columns (> columns 0))
                      (format "  columns={%d}\n" columns))
                    "/>")
            ;; Fallback if no images found
            contents)))

     ;; Handle PULLQUOTE blocks
     ((string-equal block-type "PULLQUOTE")
      (concat "<div class=\"pullquote\">\n\n"
              contents
              "\n</div>\n"))

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
            (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\|avif\\)\\s-*$" nil t)
              (let ((path (string-trim (match-string 0))))
                (when (file-exists-p path)
                  (push path images))))))))
    (nreverse images)))
(provide 'ox-astro-table-handlers)

;;; ox-astro-table-handlers.el ends here
