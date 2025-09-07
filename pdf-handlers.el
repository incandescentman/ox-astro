;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PDF HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--get-pdfs-folder (posts-folder sub-dir)
  "Compute the app's public/pdfs/{slug}/ folder using POSTS-FOLDER and SUB-DIR.
SUB-DIR for images is typically "posts/{slug}/"; for PDFs we drop the
leading "posts/" so files land in /pdfs/{slug}/."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; src dir = .../apps/<app>/src/
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir)))))
           ;; app root = parent of src
           (app-root (file-name-directory (directory-file-name src-dir)))
           (pdf-subdir (if (and sub-dir (string-prefix-p "posts/" sub-dir))
                           (substring sub-dir (length "posts/"))
                           sub-dir)))
      (expand-file-name (concat "public/pdfs/" pdf-subdir) app-root))))

(defun org-astro--collect-pdfs-from-tree (tree)
  "Collect PDF file paths from Org TREE links."
  (let (pdfs)
    (org-element-map tree 'link
      (lambda (lnk)
        (let ((type (org-element-property :type lnk))
              (p (org-element-property :path lnk)))
          (when (and p (or (string= type "file") (null type)))
            (when (and (string-suffix-p ".pdf" (downcase p))
                       ;; Accept absolute site paths and local absolute paths
                       (or (string-prefix-p "/" p)
                           (file-name-absolute-p p)))
              (push p pdfs))))))
    (delete-dups (nreverse pdfs))))

(defun org-astro--process-pdf-path (pdf-path posts-folder sub-dir &optional update-buffer)
  "Copy local PDF to app public/pdfs/SUB-DIR and optionally update source buffer.
Returns the site path beginning with /pdfs/."
  (when (and pdf-path posts-folder)
    (let* ((pdf-path (substring-no-properties pdf-path))
           ;; If already a site path, just return it
           (site-path (when (string-prefix-p "/pdfs/" pdf-path) pdf-path))
           (pdfs-folder (org-astro--get-pdfs-folder posts-folder sub-dir))
           (pdf-subdir (if (and sub-dir (string-prefix-p "posts/" sub-dir))
                           (substring sub-dir (length "posts/"))
                           sub-dir)))
      (cond
       (site-path site-path)
       ;; Local file → copy
       ((and (file-name-absolute-p pdf-path) (file-exists-p pdf-path) pdfs-folder)
        (let* ((clean-filename (org-astro--sanitize-filename (file-name-nondirectory pdf-path)))
               (target-dir pdfs-folder)
               (target-path (expand-file-name clean-filename target-dir))
               (site (concat "/pdfs/" pdf-subdir clean-filename)))
          (make-directory target-dir t)
          (condition-case err
              (progn (copy-file pdf-path target-path t)
                     (message "Copied PDF %s -> %s" pdf-path target-path))
            (error (message "Failed to copy PDF %s: %s" pdf-path err)))
          (when update-buffer
            ;; Update source buffer from old local absolute path to new absolute target path
            (ignore-errors (org-astro--update-source-buffer-image-path pdf-path target-path)))
          site))
       (t
        ;; Unknown form; leave as-is
        pdf-path)))))
