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
        (org-astro--dbg-log info "GALLERY: image-imports contains %d items" (length image-imports))
        (when image-imports
          (org-astro--dbg-log info "GALLERY: import paths: %s"
                              (mapcar (lambda (item) (plist-get item :path)) image-imports)))
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
