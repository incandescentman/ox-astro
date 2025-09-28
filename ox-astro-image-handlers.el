;;; ox-astro-image-handlers.el --- Image handling functions for ox-astro  -*- lexical-binding: t -*-

;;; Commentary:
;; Image handling functions extracted from ox-astro-helpers.el

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-astro-config)

;; Declare functions from ox-astro-helpers
(declare-function org-astro--debug-log-direct "ox-astro-helpers")
(declare-function org-astro--dbg-log "ox-astro-helpers")
(declare-function org-astro--get-assets-folder "ox-astro-helpers")
(declare-function org-astro--sanitize-filename "ox-astro-helpers")

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
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (message "[ox-astro][img] UPDATE-BUFFER: Attempting to update %s -> %s" old-path new-path)
    (message "[ox-astro][img] UPDATE-BUFFER: Current buffer: %s, file: %s, read-only: %s"
             (buffer-name) (buffer-file-name) buffer-read-only))

  ;; Strategy 1: Check if current buffer has a file name and is writable
  (let ((source-buffer nil))
    (cond
     ;; Current buffer is the source file (but NOT if it's a temp export buffer)
     ((and (buffer-file-name)
           (not buffer-read-only)
           (not (string-match-p "^ \\*temp\\*" (buffer-name))))
      (message "DEBUG: Using current buffer as source: %s" (buffer-name))
      (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
        (message "[ox-astro][img] UPDATE-BUFFER: Using current buffer as source"))
      (setq source-buffer (current-buffer)))

     ;; Try to find source buffer by examining all buffers
     (t
      (message "DEBUG: Current buffer (%s) is not suitable, searching for source buffer..." (buffer-name))
      (message "DEBUG: Current buffer file: %s, read-only: %s" (buffer-file-name) buffer-read-only)
      (org-astro--debug-log-direct "UPDATE-BUFFER: Searching for source buffer containing path: %s" old-path)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (let ((buf-file (buffer-file-name))
                (buf-readonly buffer-read-only))
            (when (and buf-file (string-match-p "\\.org$" buf-file))
              (org-astro--debug-log-direct "UPDATE-BUFFER: Checking buffer %s (file: %s, readonly: %s)"
                                           (buffer-name) buf-file buf-readonly))
            (when (and buf-file
                       (not buf-readonly)
                       (string-match-p "\\.org$" buf-file)
                       ;; Check if this buffer contains the image path (raw or bracketed)
                       (save-excursion
                         (goto-char (point-min))
                         (or (search-forward old-path nil t)
                             (search-forward (format "[[%s]]" old-path) nil t))))
              (message "DEBUG: Found source buffer: %s (%s)" (buffer-name) buf-file)
              (org-astro--debug-log-direct "UPDATE-BUFFER: FOUND source buffer: %s" (buffer-name))
              (setq source-buffer buf)
              (cl-return)))))))

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
              (org-astro--debug-log-direct "UPDATE: Calling buffer update for %s -> %s" image-path target-path)
              (let ((update-result (org-astro--update-source-buffer-image-path image-path target-path)))
                (org-astro--debug-log-direct "UPDATE: Buffer update result: %s" (if update-result "SUCCESS" "FAILED"))))

            ;; Return the alias path for imports
            (when (file-exists-p target-path)
              (let ((result (concat "~/assets/images/" sub-dir clean-filename)))
                (message "DEBUG: Returning astro path: %s" result)
                result)))))))))


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
(provide 'ox-astro-image-handlers)

;;; ox-astro-image-handlers.el ends here
