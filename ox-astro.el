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

;; Placement helper: insert keywords after org-roam preamble (- Links :: / - Source ::)
;; (Placement helper removed for now — using existing insertion helper.)


;;;###autoload
(defun org-astro-export-as-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX buffer."
  (interactive)
  (org-export-to-buffer 'astro "*Astro MDX Export*"
    async subtreep visible-only body-only))

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
               (resolved-posts-folder-raw (and posts-folder-from-file
                                               (cdr (assoc posts-folder-from-file org-astro-known-posts-folders))))
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
                         (selected-path-raw (when selection
                                              (cdr (assoc selection org-astro-known-posts-folders))))
                         ;; Trim whitespace from selected path
                         (selected-path (and selected-path-raw
                                             (string-trim selected-path-raw))))
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
               (pub-dir (when posts-folder
                          (file-name-as-directory
                           (expand-file-name (org-trim posts-folder)))))
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

          (if pub-dir
              (progn
                (make-directory pub-dir t)
                (org-export-to-file 'astro outfile async subtreep visible-only body-only)
                outfile)  ; Return the output file path
              (progn
                (message "Astro export cancelled: No posts folder selected.")
                nil))))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (table-cell . org-astro-table-cell))

  :filters-alist
  '((:filter-parse-tree . org-astro-prepare-images-filter)
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
    (:astro-date-format  nil "date-format" org-astro-date-format nil)
    (:hide-from-main     "HIDE_FROM_MAIN"     nil nil nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here
