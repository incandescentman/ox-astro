;;; ox-astro.el --- Astro MDX Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Author: Gemini & Jay Dixit
;; Version: 0.7.0
;; Package-Requires: ((emacs "26.3"))
;; Keywords: Org, markdown, docs, astro
;; URL: https://github.com/your-repo/ox-astro

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ox-astro implements a Markdown back-end for the Org export engine.
;; The exported Markdown is compatible with the Astro framework
;; (https://astro.build/) and is saved in the .mdx format.
;; This exporter generates the post front-matter in YAML format and allows
;; for component imports, which are essential for MDX.

;; To start using this exporter, add the below to your Emacs config:
;;
;;   (with-eval-after-load 'ox
;;     (require 'ox-astro))
;;
;; # Workflow
;;
;; This exporter is designed for a one-post-per-file workflow.
;; A single Org file exports to a single .mdx file. If #+DESTINATION_FOLDER is not
;; set, files are exported to a subdirectory named "astro-posts". This can
;; be customized via `org-astro-default-posts-folder`.

;;; Code:

(require 'ox-md)
(require 'org)
(require 'cl-lib)

(require 'ox-astro-config)
(require 'ox-astro-helpers)
(require 'ox-astro-handlers)
(require 'ox-astro-table-handlers)
(require 'ox-astro-image-handlers)
(require 'ox-astro-pdf-handlers)

;; Declare functions from handler modules
(declare-function org-astro--collect-images-from-tree "ox-astro-image-handlers")
(declare-function org-astro--collect-raw-images-from-tree-region "ox-astro-image-handlers")
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--collect-pdfs-from-tree "ox-astro-pdf-handlers")
(declare-function org-astro--process-pdf-path "ox-astro-pdf-handlers")

;; Placement helper: insert keywords after org-roam preamble (- Links :: / - Source ::)
;; (Placement helper removed for now — using existing insertion helper.)


;;;###autoload
(defun org-astro-export-as-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX buffer."
  (interactive)
  (if (string-equal ".mdx" (file-name-extension (buffer-file-name)))
      (message "Cannot export from an .mdx file. Run this from the source .org file.")
    (org-export-to-buffer 'astro "*Astro MDX Export*"
      async subtreep visible-only body-only)))

;;;###autoload
(defun org-astro-export-to-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX file.
If title, excerpt, or publish date are missing, they will be
generated and added to the Org source file."
  (interactive)
  (if (string-equal ".mdx" (file-name-extension (buffer-file-name)))
      (message "Cannot export from an .mdx file. Run this from the source .org file.")
      (let ((info (org-export-get-environment 'astro subtreep))
            ;; Detect if the user is currently narrowed to a subtree.
            (was-narrowed (buffer-narrowed-p))
            (buffer-modified-p nil))
        ;; Clear any stale image import state before running export filters.
        (setq org-astro--current-body-images-imports nil)
        ;; --- PREPROCESSING: Process and update all image paths BEFORE export ---
        (let* ((tree (org-element-parse-buffer))
               (posts-folder-raw (or (plist-get info :destination-folder)
                                     (plist-get info :astro-posts-folder)))
               ;; Resolve the posts folder using the same logic as in handlers
               (folder-config (and posts-folder-raw
                                   (cdr (assoc posts-folder-raw org-astro-known-posts-folders))))
               ;; Extract path from config (handle both old string and new plist formats)
               (resolved-posts-folder-raw (if (stringp folder-config)
                                              folder-config
                                            (plist-get folder-config :path)))
               (resolved-posts-folder (and resolved-posts-folder-raw
                                           (string-trim resolved-posts-folder-raw)))
               (posts-folder (cond
                              (resolved-posts-folder resolved-posts-folder)
                              ((and posts-folder-raw
                                    (file-name-absolute-p posts-folder-raw)
                                    (file-directory-p (expand-file-name posts-folder-raw)))
                               posts-folder-raw)
                              (t nil))))
          (when posts-folder
            ;; Collect image paths
            (let* ((image-paths-from-tree (org-astro--collect-images-from-tree tree))
                   (image-paths-from-raw (org-astro--collect-raw-images-from-tree-region tree))
                   (image-paths (delete-dups (append image-paths-from-tree image-paths-from-raw)))
                   ;; Get slug for post-specific folder structure
                   (title (org-astro--get-title tree info))
                   (slug (or (plist-get info :slug)
                             (let* ((title-kw (org-element-map tree 'keyword
                                                (lambda (k)
                                                  (when (string-equal "TITLE" (org-element-property :key k)) k))
                                                nil 'first-match))
                                    (title-from-headline (not title-kw)))
                               (when title-from-headline
                                 (org-astro--slugify title)))))
                   (sub-dir (if slug (concat "posts/" slug "/") "posts/"))
                  (updated-paths nil))
              ;; Process each image and update source buffer paths immediately
              (message "DEBUG: Processing %d initial images" (length image-paths))
              (dolist (path image-paths)
                (message "DEBUG: Processing initial image: %s" path)
                (let* ((astro-path (org-astro--process-image-path path posts-folder sub-dir t))
                       (clean-filename (org-astro--sanitize-filename (file-name-nondirectory path)))
                       (target-abs (when astro-path
                                     (expand-file-name clean-filename (org-astro--get-assets-folder posts-folder sub-dir)))))
                  (when target-abs
                    (push target-abs updated-paths))))
              ;; Process PDFs similarly: copy into public/pdfs and update buffer
              (let ((pdf-paths (org-astro--collect-pdfs-from-tree tree)))
                (dolist (pdf pdf-paths)
                  (ignore-errors
                    (org-astro--process-pdf-path pdf posts-folder sub-dir t))))
              ;; If we updated any paths, save the buffer and refresh the environment
              (when updated-paths
                ;; Save the buffer to preserve the path updates
                (save-buffer)
                ;; Now refresh the export environment with the updated buffer
                (setq info (org-export-get-environment 'astro subtreep))
                ;; Re-process images after downloads to ensure downloaded images appear in MDX
                (let* ((updated-tree (org-element-parse-buffer))
                       (updated-image-paths-from-tree (org-astro--collect-images-from-tree updated-tree))
                       (updated-image-paths-from-raw (org-astro--collect-raw-images-from-tree-region updated-tree))
                       (updated-image-paths (delete-dups (append updated-image-paths-from-tree updated-image-paths-from-raw)))
                       (updated-image-imports-data nil))
                  ;; Process the updated paths for import generation
                  (when posts-folder
                    (message "DEBUG: Re-processing %d images after downloads" (length updated-image-paths))
                    (dolist (path updated-image-paths)
                      (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
                             ;; Check if this is already an asset path and convert it
                             (astro-path (cond
                                          ;; If it's already in the assets folder, construct the alias path
                                          ((and assets-folder
                                                (string-match-p (regexp-quote (expand-file-name assets-folder))
                                                              (expand-file-name path)))
                                           (concat "~/assets/images/" sub-dir (file-name-nondirectory path)))
                                          ;; Otherwise, process it normally
                                          (t (org-astro--process-image-path path posts-folder sub-dir nil))))
                             (var-name (when astro-path (org-astro--path-to-var-name (file-name-nondirectory astro-path)))))
                        (message "DEBUG: Path: %s -> Astro: %s -> Var: %s" path astro-path var-name)
                        (when (and astro-path var-name)
                          (push (list :path path :astro-path astro-path :var-name var-name) updated-image-imports-data)))))
                  ;; Update the info with the new image data
                  (when updated-image-imports-data
                    (plist-put info :astro-body-images-imports (nreverse updated-image-imports-data))
                    (setq org-astro--current-body-images-imports (nreverse updated-image-imports-data))
                    (message "DEBUG: Updated image imports with %d processed images" (length updated-image-imports-data)))))))
        ;; --- Ensure essential front-matter exists, writing back if not ---
        (save-excursion
          (condition-case err
              (let* ((tree (org-element-parse-buffer))
                     (title-present (plist-get info :title))
                     (excerpt-present (or (plist-get info :astro-excerpt) (plist-get info :excerpt)))
                     (date-present (or (plist-get info :astro-publish-date) (plist-get info :publish-date) (plist-get info :date))))

                ;; 1. Handle Title and Slug
                ;; Generate title and slug when title comes from headline (no #+TITLE keyword in narrowed tree)
                (let* ((title-kw (org-element-map tree 'keyword
                                   (lambda (k)
                                     (when (string-equal "TITLE" (org-element-property :key k)) k))
                                   nil 'first-match))
                       (title-from-headline (not title-kw)))
                  (when title-from-headline
                    (let* ((headline (org-element-map tree 'headline 'identity nil 'first-match))
                           (title    (when headline
                                       (org-astro--safe-export (org-element-property :title headline) info))))
                      (when (and title (not (string-blank-p title)))
                        (org-astro--upsert-keyword-after-roam "TITLE" title)
                        (let ((slug (org-astro--slugify title)))
                          (when (and slug (not (string-blank-p slug)))
                            (org-astro--upsert-keyword-after-roam "SLUG" slug)))
                        (setq buffer-modified-p t)))))

                ;; 2. Handle Excerpt (only if missing), placed after org-roam preamble
                (unless excerpt-present
                  (let ((excerpt-text (org-astro--get-excerpt tree info)))
                    (when (and excerpt-text (not (string-blank-p excerpt-text)))
                      (org-astro--upsert-keyword-after-roam "EXCERPT" excerpt-text)
                      (setq buffer-modified-p t))))

                ;; 3. Handle Date (only if missing), placed after org-roam preamble
                (unless date-present
                  (let ((date-str (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
                    (org-astro--upsert-keyword-after-roam "PUBLISH_DATE" date-str)
                    (setq buffer-modified-p t))))
            (error (message "[ox-astro] Preflight skipped due to: %S" err))))

        ;; If we modified the buffer, save it and refresh the export environment
        (when buffer-modified-p
          (save-buffer)
          (setq info (org-export-get-environment 'astro)))

        ;; --- Original export logic continues below ---
        (let* ((posts-folder-from-file (or (plist-get info :astro-posts-folder)
                                           (plist-get info :destination-folder)))
               ;; Look up the folder config - now returns a plist
               (folder-config (cdr (assoc posts-folder-from-file org-astro-known-posts-folders)))
               ;; Extract path from the plist (handle both old and new formats)
               (resolved-posts-folder-raw (if (stringp folder-config)
                                              ;; Old format: just a string path
                                              folder-config
                                            ;; New format: plist with :path
                                            (plist-get folder-config :path)))
               ;; Extract preserve-folder-structure flag
               (preserve-folder-structure (and (listp folder-config)
                                               (plist-get folder-config :preserve-folder-structure)))
               ;; Store the selected folder nickname for later use
               (selected-folder-nickname posts-folder-from-file)
               ;; Trim whitespace from resolved path to handle configuration errors
               (resolved-posts-folder (and resolved-posts-folder-raw
                                           (string-trim resolved-posts-folder-raw)))
               (posts-folder
                (cond
                 ;; If we found it in known folders, use that path
                 (resolved-posts-folder resolved-posts-folder)
                 ;; If posts-folder-from-file exists and looks like an absolute path, use it directly
                 ((and posts-folder-from-file
                       (file-name-absolute-p posts-folder-from-file)
                       (file-directory-p (expand-file-name posts-folder-from-file)))
                  posts-folder-from-file)
                 ;; Otherwise, prompt the user for selection
                 (t
                  (let* ((selection (completing-read "Select a posts folder: "
                                                     org-astro-known-posts-folders
                                                     nil t posts-folder-from-file))
                         (selected-config (cdr (assoc selection org-astro-known-posts-folders)))
                         ;; Handle both old and new formats
                         (selected-path-raw (if (stringp selected-config)
                                               selected-config
                                             (plist-get selected-config :path)))
                         ;; Trim whitespace from selected path
                         (selected-path (and selected-path-raw
                                             (string-trim selected-path-raw))))
                    ;; Update the variables for the selected folder
                    (setq selected-folder-nickname selection)
                    (setq preserve-folder-structure (and (listp selected-config)
                                                         (plist-get selected-config :preserve-folder-structure)))
                    (when selected-path
                      ;; Add the DESTINATION_FOLDER keyword to the org file
                      (save-excursion
                        (goto-char (point-min))
                        (if (re-search-forward "^#\\+DESTINATION_FOLDER:" nil t)
                            ;; Update existing DESTINATION_FOLDER keyword
                            (progn
                              (beginning-of-line)
                              (kill-line)
                              (insert (format "#+DESTINATION_FOLDER: %s" selection)))
                            (org-astro--upsert-keyword-after-roam "DESTINATION_FOLDER" selection)))
                      (save-buffer))
                    selected-path))))
               (pub-dir-base (when posts-folder
                               (file-name-as-directory
                                (expand-file-name (org-trim posts-folder)))))
               ;; Calculate the subdirectory if preserving folder structure for this destination
               (preserved-subdir
                (when (and preserve-folder-structure
                           pub-dir-base
                           (buffer-file-name))
                  (let* ((source-file (expand-file-name (buffer-file-name)))
                         (source-root (expand-file-name org-astro-source-root-folder))
                         (relative-path nil))
                    ;; Check if source file is under the configured source root
                    (when (string-prefix-p source-root source-file)
                      (setq relative-path (file-relative-name source-file source-root)))
                    ;; Extract directory part of relative path (remove filename)
                    (when relative-path
                      (let ((dir (file-name-directory relative-path)))
                        (when (and dir (not (string= dir "./")))
                          dir))))))
               ;; Combine base directory with preserved subdirectory
               (pub-dir (if preserved-subdir
                           (file-name-as-directory
                            (expand-file-name preserved-subdir pub-dir-base))
                         pub-dir-base))
               (default-outfile (org-export-output-file-name ".mdx" subtreep pub-dir))
               (out-dir (file-name-directory default-outfile))
               (out-filename (file-name-nondirectory default-outfile))
               ;; Prefer a SLUG found in the narrowed region (subtree) first,
               ;; then fall back to searching the full buffer.
               (slug-filename (let ((slug-in-narrow
                                     (save-excursion
                                       (goto-char (point-min))
                                       (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" (point-max) t)
                                         (org-trim (match-string 1)))))
                                    (slug-in-full
                                     (save-excursion
                                       (save-restriction
                                         (widen)
                                         (goto-char (point-min))
                                         (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" nil t)
                                           (org-trim (match-string 1)))))))
                                (or slug-in-narrow slug-in-full)))
               (final-filename
                ;; Treat either explicit subtree export or an actively narrowed buffer
                ;; as a signal to use the subtree slug for the filename.
                (if (and (or subtreep was-narrowed)
                         slug-filename (not (string-blank-p slug-filename)))
                    ;; Use slug as filename for subtree exports
                    (concat slug-filename ".mdx")
                    ;; Use default filename processing for full file exports
                    (replace-regexp-in-string
                     "_" "-"
                     (replace-regexp-in-string "^[0-9]+-" "" out-filename))))
               (outfile (expand-file-name final-filename out-dir)))

          ;; Update debug system with actual output file path now that we know it
          (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
            (org-astro--debug-log-direct "Export starting - Output file: %s" outfile)
            (org-astro--dbg-update-output-file info outfile))

          (if pub-dir
              (progn
                (make-directory pub-dir t)
                ;; First export pass
                (message "Running first export pass...")
                (org-export-to-file 'astro outfile async subtreep visible-only body-only)
                ;; Clear import state for second pass
                (setq org-astro--current-body-images-imports nil)
                ;; Second export pass to ensure all images appear
                (message "Running second export pass to ensure complete image processing...")
                (org-export-to-file 'astro outfile async subtreep visible-only body-only)
                ;; Log completion and ensure clipboard copy
                (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                  (org-astro--debug-log-direct "Export complete: %s" outfile)
                  ;; Copy file paths to clipboard
                  (let* ((source-file (buffer-file-name))
                         (debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-debug.log"))
                         (clipboard-text (format "Source: %s\nOutput: %s\nDebug: %s"
                                                 source-file outfile debug-file))
                         (pbcopy (executable-find "pbcopy")))
                    (when pbcopy
                      (condition-case _
                          (with-temp-buffer
                            (insert clipboard-text)
                            (call-process-region (point-min) (point-max) pbcopy nil nil nil)
                            (message "File paths copied to clipboard!"))
                        (error nil)))))
                (message "Export complete! All images should now be visible.")
                outfile)  ; Return the output file path
              (progn
                (message "Astro export cancelled: No posts folder selected.")
                nil)))))));
;;; Backend Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'astro 'md
  :menu-entry
  '(?a "Export to Astro"
       ((?a "As MDX buffer" org-astro-export-as-mdx)
        (?x "To MDX file" org-astro-export-to-mdx)
        (?o "To MDX file and open"
            (lambda (_a _s _v _b)
              (org-open-file
               (org-astro-export-to-mdx))))))

  :translate-alist
  '((src-block . org-astro-src-block)
    (link . org-astro-link)
    (headline . org-astro-heading)
    (paragraph . org-astro-paragraph)
    (plain-text . org-astro-plain-text)
    (subscript . org-astro-subscript)
    (table . org-astro-table)
    (table-row . org-astro-table-row)
    (table-cell . org-astro-table-cell)
    (special-block . org-astro-special-block))

  :filters-alist
  '((:filter-parse-tree . (org-astro-auto-wrap-image-paths-filter
                           org-astro-prepare-images-filter))
    (:filter-body . org-astro-body-filter)
    (:filter-final-output . org-astro-final-output-filter))

  :options-alist
  '((:smart-quotes       nil                   org-md-use-smart-quotes nil)
    (:title              "TITLE"               nil nil nil)
    (:slug               "SLUG"                nil nil nil)
    (:author             "AUTHOR"              nil nil nil)
    (:author-image       "AUTHOR_IMAGE"        nil nil nil)
    (:date               "DATE"                nil nil nil)
    (:publish-date       "PUBLISH_DATE"        nil nil nil)
    (:excerpt            "EXCERPT"             nil nil nil)
    (:tags               "TAGS"                nil nil 'newline)
    (:categories         "CATEGORIES"          nil nil 'newline)
    (:cover-image        "COVER_IMAGE"         nil nil nil)
    (:cover-image-alt    "COVER_IMAGE_ALT"     nil nil nil)
    (:visibility         "VISIBILITY"          nil nil nil)
    (:status             "STATUS"              nil nil nil)
    (:destination-folder "DESTINATION_FOLDER"  nil nil nil)
    (:astro-publish-date "ASTRO_PUBLISH_DATE"  nil nil nil)
    (:astro-excerpt      "ASTRO_EXCERPT"       nil nil nil)
    (:astro-image        "ASTRO_IMAGE"         nil nil nil)
    (:astro-image-alt    "ASTRO_IMAGE_ALT"     nil nil nil)
    (:astro-author-image "ASTRO_AUTHOR_IMAGE"  nil nil nil)
    (:astro-tags         "ASTRO_TAGS"          nil nil 'newline)
    (:astro-categories   "ASTRO_CATEGORIES"    nil nil 'newline)
    (:astro-imports      "ASTRO_IMPORTS"       nil nil 'newline)
    (:astro-posts-folder "ASTRO_POSTS_FOLDER"  nil nil nil)
    (:astro-date-format  nil "date-format" org-astro-date-format nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here
