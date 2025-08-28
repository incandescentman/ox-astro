;;; update-image-paths.el --- Update image paths in org-mode files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: ox-astro
;; Keywords: convenience, files
;; Version: 0.1.0

;;; Commentary:

;; This file provides functions to update image paths in org-mode source files,
;; replacing absolute paths with processed Astro-compatible paths.

;;; Code:

(require 'org)

(defun update-image-paths-collect-raw-images ()
  "Collect all raw image paths from the current buffer."
  (let (images)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
        (let ((path (string-trim (match-string 0))))
          (when (and path (not (member path images)))
            (push path images)))))
    (nreverse images)))

(defun update-image-paths-process-single-path (old-path posts-folder slug)
  "Process a single image path OLD-PATH for POSTS-FOLDER and SLUG.
Returns the new astro-compatible path, or nil if processing failed."
  (when (and old-path (file-exists-p old-path) posts-folder slug)
    (let* ((filename (file-name-nondirectory old-path))
           ;; Clean filename for astro compatibility
           (clean-filename (replace-regexp-in-string "[^a-zA-Z0-9._-]" "-" filename))
           (assets-dir (expand-file-name 
                       (concat "src/assets/images/posts/" slug "/")
                       (file-name-directory posts-folder)))
           (target-path (expand-file-name clean-filename assets-dir))
           (astro-alias (concat "~/assets/images/posts/" slug "/" clean-filename)))
      ;; Create directory if needed
      (make-directory assets-dir t)
      ;; Copy file
      (when (copy-file old-path target-path t)
        astro-alias))))

(defun update-image-paths-replace-in-buffer (old-path new-path)
  "Replace OLD-PATH with NEW-PATH in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (old-escaped (regexp-quote old-path)))
      (while (re-search-forward (concat "^\\s-*" old-escaped "\\s-*$") nil t)
        (replace-match new-path)))))

(defun update-image-paths-get-slug-from-buffer ()
  "Extract slug from current buffer, either from #+SLUG or #+TITLE."
  (save-excursion
    (goto-char (point-min))
    (or
     ;; First try #+SLUG
     (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" nil t)
       (string-trim (match-string 1)))
     ;; Then try #+TITLE and slugify it
     (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
       (let ((title (string-trim (match-string 1))))
         (downcase 
          (replace-regexp-in-string 
           "[^a-zA-Z0-9]+" "-" 
           (replace-regexp-in-string "^-+\\|-+$" "" title))))))))

(defun update-image-paths-get-posts-folder-from-buffer ()
  "Extract posts folder from #+DESTINATION_FOLDER in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+DESTINATION_FOLDER:\\s-*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun update-image-paths-interactive ()
  "Interactively update all image paths in the current org-mode buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command can only be used in org-mode buffers"))
  
  (let* ((raw-images (update-image-paths-collect-raw-images))
         (posts-folder (update-image-paths-get-posts-folder-from-buffer))
         (slug (update-image-paths-get-slug-from-buffer))
         (updates-made 0))
    
    (unless raw-images
      (message "No raw image paths found in buffer")
      (return))
      
    (unless posts-folder
      (user-error "No #+DESTINATION_FOLDER found in buffer"))
      
    (unless slug
      (user-error "No #+SLUG or #+TITLE found in buffer to generate slug"))
      
    (message "Found %d raw image paths to process..." (length raw-images))
    
    (dolist (old-path raw-images)
      (let ((new-path (update-image-paths-process-single-path old-path posts-folder slug)))
        (if new-path
            (progn
              (update-image-paths-replace-in-buffer old-path new-path)
              (setq updates-made (1+ updates-made))
              (message "Updated: %s -> %s" old-path new-path))
          (message "Failed to process: %s" old-path))))
    
    (if (> updates-made 0)
        (progn
          (save-buffer)
          (message "Successfully updated %d image paths" updates-made))
      (message "No image paths were updated"))))

(defun update-image-paths-batch (org-file posts-folder)
  "Batch update image paths in ORG-FILE for POSTS-FOLDER.
Returns number of paths updated."
  (with-current-buffer (find-file-noselect org-file)
    (let* ((raw-images (update-image-paths-collect-raw-images))
           (slug (update-image-paths-get-slug-from-buffer))
           (updates-made 0))
      
      (when (and raw-images slug)
        (dolist (old-path raw-images)
          (let ((new-path (update-image-paths-process-single-path old-path posts-folder slug)))
            (when new-path
              (update-image-paths-replace-in-buffer old-path new-path)
              (setq updates-made (1+ updates-made)))))
        
        (when (> updates-made 0)
          (save-buffer)))
      
      updates-made)))

(provide 'update-image-paths)

;;; update-image-paths.el ends here