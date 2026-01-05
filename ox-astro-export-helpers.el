;;; ox-astro-export-helpers.el --- Export helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:
(require 'org-element)
(require 'subr-x)

;; Declare functions from other modules
(declare-function org-astro--build-image-manifest "ox-astro-image-handlers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--collect-images-from-tree (tree &optional info)
  "Collect all image paths from the parse TREE.
When INFO is provided, forward it to the manifest builder so callers
can share a single discovery pass."
  (let ((manifest (org-astro--build-image-manifest tree info)))
    (mapcar (lambda (entry) (plist-get entry :original-path))
            manifest)))

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


;;;; Stale MDX Cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--extract-orgpath-from-mdx (file)
  "Extract orgPath value from MDX FILE's frontmatter.
Returns the path string or nil if not found."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 2000)  ; Read first 2KB
        (goto-char (point-min))
        (when (looking-at "---")
          (forward-line 1)
          (let ((end (save-excursion
                       (when (re-search-forward "^---$" nil t)
                         (point)))))
            (when end
              (when (re-search-forward "^orgPath:\\s-*\\(.+\\)$" end t)
                (string-trim (match-string 1)))))))
    (error nil)))

(defun org-astro--cleanup-stale-mdx-files (output-dir source-file current-outfile)
  "Delete MDX files in OUTPUT-DIR from SOURCE-FILE but different from CURRENT-OUTFILE.
Scans all .mdx files in OUTPUT-DIR, checks their orgPath frontmatter field,
and deletes any that match SOURCE-FILE but have a different filename.
Returns a list of deleted file paths."
  (when (and output-dir source-file current-outfile
             (file-directory-p output-dir))
    (let ((deleted-files nil)
          (source-file-expanded (expand-file-name source-file))
          (current-outfile-expanded (expand-file-name current-outfile)))
      (dolist (mdx-file (directory-files output-dir t "\\.mdx$"))
        (unless (string= (expand-file-name mdx-file) current-outfile-expanded)
          (let ((orgpath (org-astro--extract-orgpath-from-mdx mdx-file)))
            (when (and orgpath
                       (string= (expand-file-name orgpath) source-file-expanded))
              (condition-case err
                  (progn
                    (delete-file mdx-file)
                    (push mdx-file deleted-files)
                    (message "[ox-astro] Deleted stale MDX: %s" (file-name-nondirectory mdx-file)))
                (error
                 (message "[ox-astro] Failed to delete %s: %s" mdx-file err)))))))
      (nreverse deleted-files))))

(provide 'ox-astro-export-helpers)
;;; ox-astro-export-helpers.el ends here
