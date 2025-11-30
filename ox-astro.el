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

(defcustom org-astro-debug-log-file (expand-file-name "ox-astro-debug.log" temporary-file-directory)
  "File path for writing debug logs when `org-astro-debug-images` is non-nil."
  :group 'org-export-astro
  :type 'file)

(defcustom org-astro-debug-console nil
  "When non-nil, emit verbose debug logging to *Messages* during export."
  :group 'org-export-astro
  :type 'boolean)

(defcustom org-astro-copy-to-clipboard nil
  "When non-nil, copy source/output/debug paths to the clipboard after export."
  :group 'org-export-astro
  :type 'boolean)

;; Declare functions from handler modules
(declare-function org-astro--collect-images-from-tree "ox-astro-image-handlers")
(declare-function org-astro--build-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--collect-raw-images-from-tree-region "ox-astro-image-handlers")
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--collect-pdfs-from-tree "ox-astro-pdf-handlers")
(declare-function org-astro--process-pdf-path "ox-astro-pdf-handlers")

;; Placement helper: insert keywords after org-roam preamble (- Links :: / - Source ::)
;; (Placement helper removed for now — using existing insertion helper.)

;; Export hook management -----------------------------------------------------

(defvar org-astro--parsing-hook-blocklist
  '(org-export-id-link-removal)
  "Functions removed from Org export parsing hooks during Astro exports.
This prevents external configuration from stripping org-roam ID links before
our custom transcoders run.")

(defvar org-astro--mdx-export-active nil
  "Guard to prevent re-entrant MDX exports within the same command.")

(defun org-astro--sanitize-export-hook-list (hooks)
  "Return HOOKS without entries listed in `org-astro--parsing-hook-blocklist'.
The return value is a cons cell (SANITIZED . REMOVED) where REMOVED holds any
functions filtered out."
  (let ((sanitized (copy-sequence (or hooks '())))
        (removed nil))
    (dolist (fn org-astro--parsing-hook-blocklist)
      (when (memq fn sanitized)
        (setq sanitized (delq fn sanitized))
        (push fn removed)))
    (cons sanitized (nreverse removed))))

(defmacro org-astro--with-export-sanitization (&rest body)
  "Execute BODY with export hooks sanitized for Astro-specific requirements."
  (declare (indent 0) (debug t))
  `(let* ((processing-sanitization
           (org-astro--sanitize-export-hook-list org-export-before-processing-functions))
          (org-export-before-processing-functions (car processing-sanitization))
          (org-export-before-processing-hook org-export-before-processing-functions)
          (parsing-sanitization
           (org-astro--sanitize-export-hook-list org-export-before-parsing-functions))
          (org-export-before-parsing-functions (car parsing-sanitization))
          (org-export-before-parsing-hook org-export-before-parsing-functions))
     (let ((removed-processing (cdr processing-sanitization))
           (removed-parsing (cdr parsing-sanitization)))
       (when removed-processing
         (org-astro--debug-log-direct "Disabled before-processing hook(s): %S"
                                      removed-processing))
       (when removed-parsing
         (org-astro--debug-log-direct "Disabled before-parsing hook(s): %S"
                                      removed-parsing)))
     ,@body))


;;;###autoload
(defun org-astro-export-as-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX buffer."
  (interactive)
  (if (string-equal ".mdx" (file-name-extension (buffer-file-name)))
      (message "Cannot export from an .mdx file. Run this from the source .org file.")
      (org-astro--with-export-sanitization
        (let ((org-astro--export-in-progress t))
          (org-export-to-buffer 'astro "*Astro MDX Export*"
            async subtreep visible-only body-only)))))

;;;###autoload
(defun org-astro-export-to-mdx (&optional async subtreep visible-only body-only)
  "Export current buffer to an Astro-compatible MDX file.
If title, excerpt, or publish date are missing, they will be
generated and added to the Org source file."
  (interactive)
  (if (string-equal ".mdx" (file-name-extension (buffer-file-name)))
      (message "Cannot export from an .mdx file. Run this from the source .org file.")
      (if org-astro--mdx-export-active
          org-astro--current-outfile
        (let ((org-astro--mdx-export-active t))
          (org-astro--with-export-sanitization
        (save-restriction
          (let* ((was-narrowed (buffer-narrowed-p))
                 (narrow-start (when was-narrowed (point-min)))
                 (narrow-end (when was-narrowed (point-max)))
                 (effective-subtreep (or subtreep was-narrowed))
                 ;; Always derive export metadata from the start of the narrowed
                 ;; region so we export the intended subtree regardless of point.
                 (info (save-excursion
                         (when narrow-start (goto-char narrow-start))
                         (org-export-get-environment 'astro effective-subtreep)))
                 (buffer-modified-p nil))
          ;; DEBUG: Check buffer at very start (console-gated)
          (when org-astro-debug-console
            (save-excursion
              (goto-char (point-min))
              (let ((id-link-count 0))
                (while (re-search-forward "\\[\\[id:" nil t)
                  (setq id-link-count (1+ id-link-count)))
                (message "[DEBUG-START] At function start: Found %d [[id: patterns in buffer" id-link-count))))
          ;; Clear any stale image import state before running export filters.
          (setq org-astro--current-body-images-imports nil)
          ;; --- AUTO-NORMALIZE: Convert org headings to markdown in user/prompt/quote blocks ---
          ;; This must run BEFORE org-mode parses the buffer, otherwise asterisks at start
          ;; of lines inside src blocks will be interpreted as org headlines and break the block.
          (org-astro--normalize-user-blocks)
          ;; DEBUG: Check buffer after normalize (console-gated)
          (when org-astro-debug-console
            (save-excursion
              (goto-char (point-min))
              (let ((id-link-count 0))
                (while (re-search-forward "\\[\\[id:" nil t)
                  (setq id-link-count (1+ id-link-count)))
                (message "[DEBUG-AFTER-NORMALIZE] After normalize: Found %d [[id: patterns in buffer" id-link-count))))
          ;; --- PREPROCESSING: Process and update all image paths BEFORE export ---
          (let* ((tree (org-element-parse-buffer))
                 (destination-keyword (org-astro--keyword-value tree '("DESTINATION_FOLDER" "DESTINATION-FOLDER")))
                 (posts-folder-raw-input (or (plist-get info :destination-folder)
                                             (plist-get info :astro-posts-folder)
                                             destination-keyword))
                 ;; Trim whitespace from the folder name to handle config errors
                 (posts-folder-raw (and posts-folder-raw-input
                                        (string-trim posts-folder-raw-input)))
                 ;; Resolve the posts folder using the same logic as in handlers
                 ;; Use a more forgiving lookup that ignores whitespace differences
                 (folder-config (and posts-folder-raw
                                     (cdr (cl-find posts-folder-raw org-astro-known-posts-folders
                                                   :test (lambda (needle pair)
                                                           (string= needle (string-trim (car pair))))))))
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
            (when posts-folder-raw
              (setq info (plist-put info :destination-folder posts-folder-raw)))
            (when posts-folder
              (let* ((title (org-astro--get-title tree info))
                     (slug (or (plist-get info :slug)
                               (when title
                                 (org-astro--slugify title))))
                     (sub-dir (if slug (concat "posts/" slug "/") "posts/"))
                     (image-manifest (org-astro--build-image-manifest tree info))
                     (process-result (org-astro--process-image-manifest image-manifest posts-folder sub-dir))
                     (processed (plist-get process-result :entries))
                     (context (list :manifest image-manifest
                                    :processed processed
                                    :posts-folder posts-folder
                                    :sub-dir sub-dir)))
                (setq info (cl-putf info :astro-image-manifest image-manifest))
                (setq info (cl-putf info :astro-export-context context))
                (when processed
                  (plist-put info :astro-body-images-imports processed)
                  (setq org-astro--current-body-images-imports processed))
                ;; Process PDFs similarly: copy into public/pdfs and update buffer
                (let ((pdf-paths (org-astro--collect-pdfs-from-tree tree)))
                  (dolist (pdf pdf-paths)
                    (condition-case err
                        (org-astro--process-pdf-path pdf posts-folder sub-dir t)
                      (error (message "ERROR processing PDF %s: %s" pdf err)))))
                (when (plist-get process-result :buffer-modified)
                  (setq buffer-modified-p t)
                  (setq info (save-excursion
                              (when narrow-start (goto-char narrow-start))
                              (org-export-get-environment 'astro effective-subtreep)))
                  (setq tree (org-element-parse-buffer))
                  (let* ((refreshed-manifest (org-astro--build-image-manifest tree info))
                         (refreshed-result (org-astro--process-image-manifest refreshed-manifest posts-folder sub-dir '(:update-buffer nil)))
                         (refreshed-processed (plist-get refreshed-result :entries))
                         (refreshed-context (list :manifest refreshed-manifest
                                                  :processed refreshed-processed
                                                  :posts-folder posts-folder
                                                  :sub-dir sub-dir)))
                    (setq info (cl-putf info :astro-image-manifest refreshed-manifest))
                    (setq info (cl-putf info :astro-export-context refreshed-context))
                    (when refreshed-processed
                      (plist-put info :astro-body-images-imports refreshed-processed)
                      (setq org-astro--current-body-images-imports refreshed-processed)))))))
          ;; --- Ensure essential front-matter exists, writing back if not ---
          (save-excursion
            (condition-case err
                (let* ((tree (org-element-parse-buffer))
                       (title-present (plist-get info :title))
                       (excerpt-present (or (plist-get info :astro-excerpt) (plist-get info :excerpt)))
                       (date-present (or (plist-get info :astro-publish-date) (plist-get info :publish-date) (plist-get info :date))))

                  ;; 1. Handle Title and Slug
                  ;; First handle title generation from headline if no #+TITLE keyword
                  (let* ((title-values (org-element-map tree 'keyword
                                        (lambda (k)
                                          (when (string-equal "TITLE" (org-element-property :key k))
                                            (org-element-property :value k)))))
                         (title-values (delq nil title-values))
                         (non-date-title (cl-find-if (lambda (v) (not (org-astro--date-string-p v)))
                                                     (reverse title-values)))
                         (first-title (car title-values))
                         (title-kw (org-element-map tree 'keyword
                                     (lambda (k)
                                       (when (string-equal "TITLE" (org-element-property :key k)) k))
                                     nil 'first-match))
                         (slug-kw (org-element-map tree 'keyword
                                    (lambda (k)
                                      (when (string-equal "SLUG" (org-element-property :key k)) k))
                                    nil 'first-match))
                         (title-from-headline (not title-kw))
                         (title-val (or non-date-title first-title))
                         (input-file (or (plist-get info :input-file) (buffer-file-name)))
                         (filename-base (and input-file (file-name-base input-file)))
                         (headline (org-element-map tree 'headline 'identity nil 'first-match))
                         (headline-title (when headline
                                           (org-astro--safe-export (org-element-property :title headline) info)))
                         (date-only-title (and (null non-date-title)
                                               (or (and first-title (org-astro--date-string-p first-title))
                                                   (and filename-base (org-astro--date-string-p filename-base)))))
                         (effective-title (cond
                                           (non-date-title non-date-title)
                                           (date-only-title headline-title)
                                           (first-title first-title)
                                           (headline-title headline-title)
                                           (t nil))))
                    ;; Replace date-only titles (or date-named files) with first headline title.
                    (when (and date-only-title headline-title
                               (not (string-blank-p headline-title)))
                      (org-astro--upsert-keyword-after-roam "TITLE" headline-title)
                      (setq buffer-modified-p t)
                      (setq title-from-headline nil))
                    ;; Add title from headline if missing
                    (when title-from-headline
                      (let* ((headline (org-element-map tree 'headline 'identity nil 'first-match))
                             (title    (when headline
                                         (org-astro--safe-export (org-element-property :title headline) info))))
                        (when (and title (not (string-blank-p title)))
                          (org-astro--upsert-keyword-after-roam "TITLE" title)
                          (setq buffer-modified-p t))))

                    ;; ALWAYS add slug if missing (whether title comes from keyword or headline)
                    (unless slug-kw
                      (let* ((title (or effective-title
                                        (plist-get info :title)
                                        (org-astro--get-title tree info)))
                             (slug (when title (org-astro--slugify title))))
                        (when (and slug (not (string-blank-p slug)))
                          (org-astro--upsert-keyword-after-roam "SLUG" slug)
                          (setq buffer-modified-p t)))))

                  ;; 2. Handle Excerpt (only if missing), placed after org-roam preamble
                  ;; Insert as SUBHED (alias for excerpt) to match legacy convention
                  (unless excerpt-present
                    (let ((excerpt-text (org-astro--get-excerpt tree info)))
                      (when (and excerpt-text (not (string-blank-p excerpt-text)))
                        (org-astro--upsert-keyword-after-roam "SUBHED" excerpt-text)
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
            (setq info (save-excursion
                        (when narrow-start (goto-char narrow-start))
                        (org-export-get-environment 'astro effective-subtreep))))

          ;; --- Original export logic continues below ---
          (let* ((posts-folder-from-file-raw (or (plist-get info :astro-posts-folder)
                                                 (plist-get info :destination-folder)))
                 ;; Trim whitespace from the folder name to handle config errors
                 (posts-folder-from-file (and posts-folder-from-file-raw
                                              (string-trim posts-folder-from-file-raw)))
                 ;; Look up the folder config - now returns a plist
                 ;; Use a more forgiving lookup that ignores whitespace differences
                 (folder-config (cdr (cl-find posts-folder-from-file org-astro-known-posts-folders
                                              :test (lambda (needle pair)
                                                      (string= needle (string-trim (car pair)))))))
                 ;; Extract path from the plist (handle both old and new formats)
                 (resolved-posts-folder-raw (if (stringp folder-config)
                                                ;; Old format: just a string path
                                                folder-config
                                                ;; New format: plist with :path
                                                (plist-get folder-config :path)))
                 ;; Extract preserve-folder-structure flag
                 (preserve-folder-structure (and (listp folder-config)
                                                 (plist-get folder-config :preserve-folder-structure)))
                 ;; Extract id-link-base-path from folder config (for absolute routes)
                 (id-link-base-path-from-config (and (listp folder-config)
                                                     (plist-get folder-config :id-link-base-path)))
                 ;; Store the selected folder nickname for later use
                 (selected-folder-nickname posts-folder-from-file)
                 ;; Trim whitespace from resolved path to handle configuration errors
                 (resolved-posts-folder (and resolved-posts-folder-raw
                                             (string-trim resolved-posts-folder-raw)))
                 (posts-folder
                  (cond
                   ;; If we found it in known folders, use that path (no prompt needed)
                   (resolved-posts-folder resolved-posts-folder)
                   ;; If posts-folder-from-file exists and looks like an absolute path, use it directly
                   ((and posts-folder-from-file
                         (file-name-absolute-p posts-folder-from-file)
                         (file-directory-p (expand-file-name posts-folder-from-file)))
                    posts-folder-from-file)
                   ;; If posts-folder-from-file is specified (e.g., nickname) but wasn't resolved above,
                   ;; it means the nickname doesn't exist in org-astro-known-posts-folders
                   ;; In this case, we should NOT prompt - just fail with an error message
                   (posts-folder-from-file
                    (error "DESTINATION_FOLDER '%s' not found in org-astro-known-posts-folders. Please check your configuration." posts-folder-from-file))
                   ;; Only prompt if no DESTINATION_FOLDER was specified at all
                   (t
                    (let* ((selection (completing-read "Select a posts folder: "
                                                       org-astro-known-posts-folders
                                                       nil t))
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
                      (setq id-link-base-path-from-config (and (listp selected-config)
                                                               (plist-get selected-config :id-link-base-path)))
                      (when selected-path
                        ;; Add the DESTINATION_FOLDER keyword to the org file
                        (save-excursion
                          (goto-char (point-min))
                          (if (re-search-forward "^#\\+DESTINATION\\(?:[_-]FOLDER\\)?:" nil t)
                              ;; Update existing DESTINATION keyword, preserving user's format preference
                              (let* ((matched (match-string 0))
                                     (keyword (cond
                                               ((string-match-p "DESTINATION-FOLDER" matched) "DESTINATION-FOLDER")
                                               ((string-match-p "DESTINATION_FOLDER" matched) "DESTINATION_FOLDER")
                                               (t "DESTINATION"))))
                                (beginning-of-line)
                                (kill-line)
                                (insert (format "#+%s: %s" keyword selection)))
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
                           (source-root-raw (org-astro--effective-source-root
                                             org-astro-source-root-folder source-file))
                           (source-root (and source-root-raw (expand-file-name source-root-raw)))
                           (relative-path nil)
                           (source-root-dir (and source-root
                                                 (file-name-as-directory
                                                  (expand-file-name source-root)))))
                      ;; Check if source file is under the effective source root
                      (when (and source-root-dir
                                 (string-prefix-p source-root-dir
                                                  (file-name-directory source-file)))
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
                 (default-outfile (org-export-output-file-name ".mdx" effective-subtreep pub-dir))
                 (out-dir (file-name-directory default-outfile))
                 (out-filename (file-name-nondirectory default-outfile))
                 ;; Prefer a SLUG found in the narrowed region (subtree) first,
                 ;; then fall back to searching the full buffer, and finally use
                 ;; the value currently in the export environment.
                 (slug-filename (let ((slug-in-narrow
                                       (save-excursion
                                         (goto-char (point-min))
                                         (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" (point-max) t)
                                           (org-trim (match-string 1)))))
                                      (slug-in-full
                                       (save-excursion
                                         (save-restriction)
                                         (widen)
                                         (goto-char (point-min))
                                         (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" nil t)
                                           (org-trim (match-string 1)))))
                                      (slug-in-info (let ((val (plist-get info :slug)))
                                                      (when (and val (stringp val))
                                                        (org-trim val)))))
                                  (or slug-in-narrow slug-in-full slug-in-info)))
                 (final-filename
                  ;; Prefer slug-based filenames whenever we have a usable slug,
                  ;; regardless of whether we're exporting a subtree or full file.
                  (if (and slug-filename (not (string-blank-p slug-filename)))
                      (concat slug-filename ".mdx")
                      ;; Fallback to default filename processing when no slug exists.
                      (replace-regexp-in-string
                       "_" "-"
                       (replace-regexp-in-string "^[0-9]+-" "" out-filename))))
                 (outfile (expand-file-name final-filename out-dir)))

(defcustom org-astro-debug-log-file (expand-file-name "ox-astro-debug.log" temporary-file-directory)
  "File path for writing debug logs when `org-astro-debug-images` is non-nil."
  :group 'org-export-astro
  :type 'file)

            ;; Update debug system with actual output file path now that we know it
            (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
              (org-astro--debug-log-direct "Export starting - Output file: %s" outfile)
              (org-astro--dbg-update-output-file info outfile))

            (let* ((org-astro--export-in-progress t)  ; Signal that astro export is active
                   (org-astro--id-path-map
                    (org-astro--ensure-id-map org-astro-source-root-folder
                                              (and (buffer-file-name)
                                                   (expand-file-name (buffer-file-name)))))
                   (org-astro--current-outfile outfile)
                   (org-astro--current-output-root pub-dir-base)
                   ;; Use :id-link-base-path from destination config if present
                   (org-astro--current-id-link-base-path id-link-base-path-from-config)
                   (org-astro--broken-link-accumulator (make-hash-table :test #'equal))
                   (org-astro--broken-link-warnings-issued (make-hash-table :test #'equal)))
              (prog1
                  (if pub-dir
                      (progn
                        (make-directory pub-dir t)
                        ;; First export pass
                        (when org-astro-debug-console
                          (message "Running first export pass..."))
                        (when org-astro-debug-console
                          (message "[DEBUG-EXPORT] Calling org-export-to-file with backend: astro")
                          (message "[DEBUG-EXPORT] Backend details: %S" (org-export-get-backend 'astro)))
                        ;; DEBUG: Check if ID links exist in buffer before export
                        (save-excursion
                          (goto-char (point-min))
                          (let ((id-link-count 0))
                            (while (re-search-forward "\\[\\[id:" nil t)
                              (setq id-link-count (1+ id-link-count)))
                        (when org-astro-debug-console
                          (message "[DEBUG-BUFFER] Found %d [[id: patterns in buffer before export" id-link-count))))
                        (save-excursion
                          (when narrow-start (goto-char narrow-start))
                          (org-export-to-file 'astro outfile async effective-subtreep visible-only body-only))
                        ;; Clear import state for second pass
                        (setq org-astro--current-body-images-imports nil)
                        ;; Second export pass to ensure complete image processing
                        (when org-astro-debug-console
                          (message "Running second export pass to ensure complete image processing..."))
                        (save-excursion
                          (when narrow-start (goto-char narrow-start))
                          (org-export-to-file 'astro outfile async effective-subtreep visible-only body-only))
                        ;; Persist any broken ID links detected during export
                        (when org-astro--broken-link-accumulator
                          (org-astro--write-broken-link-report org-astro--broken-link-accumulator
                                                               org-astro--current-output-root))
                        ;; Log completion
                        (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                          (org-astro--debug-log-direct "Export complete: %s" outfile))
                        ;; Copy file paths to clipboard (opt-in)
                        (when (and (boundp 'org-astro-copy-to-clipboard) org-astro-copy-to-clipboard)
                          (let* ((source-file (buffer-file-name))
                                 (debug-file (expand-file-name org-astro-debug-log-file))
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
                        (when org-astro-debug-console
                          (message "Export complete! All images should now be visible."))
                        outfile)  ; Return the output file path
                    (progn
                      (message "Astro export cancelled: No posts folder selected.")
                      nil))
                (when (and was-narrowed narrow-start narrow-end)
                  (narrow-to-region narrow-start narrow-end))))))))))))
;;; Backend Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Advice to prevent org-export from resolving cross-file ID links to plain text
;; during the buffer copy phase. This preserves the [[id:...][...]] syntax so our
;; custom link transcoder can handle them.
(defvar org-astro--export-in-progress nil
  "Flag to track when astro export is in progress.")

(defun org-astro--preserve-id-links (orig-fun link info &optional search)
  "Advice for `org-export-resolve-id-link' to preserve cross-file ID links during astro export.
When astro export is active, return nil for cross-file ID links so they aren't
resolved to plain text in the temporary export buffer."
  (if org-astro--export-in-progress
      ;; During astro export, only resolve internal links (within same file)
      (let ((id (org-element-property :path link)))
        (when id
          (let ((match (org-id-find id)))
            (when match
              (let ((file (car match)))
                ;; Only resolve if it's in the current file
                (when (and file (string= file (buffer-file-name (buffer-base-buffer))))
                  (funcall orig-fun link info search)))))))
      ;; Not during astro export, use original behavior
      (funcall orig-fun link info search)))

(advice-add 'org-export-resolve-id-link :around #'org-astro--preserve-id-links)

(defun org-astro-link-wrapper (link desc info)
  "Debug wrapper around org-astro-link to verify it's being called."
  (when org-astro-debug-console
    (message "[DEBUG-WRAPPER] org-astro-link-wrapper CALLED! type=%s path=%s"
             (org-element-property :type link)
             (org-element-property :path link)))
  (org-astro-link link desc info))

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
    (link . org-astro-link-wrapper)
    (headline . org-astro-heading)
    (paragraph . org-astro-paragraph)
    (plain-text . org-astro-plain-text)
    (underline . org-astro-underline)
    (subscript . org-astro-subscript)
    (item . org-astro-item)
    (table . org-astro-table)
    (table-row . org-astro-table-row)
    (table-cell . org-astro-table-cell)
    (special-block . org-astro-special-block)
    (export-block . org-astro-export-block)
    (keyword . org-astro-keyword))

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
    (:date-occurred      "DATE_OCCURRED"       nil nil nil)
    (:excerpt            "EXCERPT"             nil nil nil)
    (:excerpt            "SUBHED"              nil nil nil)
    (:tags               "TAGS"                nil nil 'newline)
    (:categories         "CATEGORIES"          nil nil 'newline)
    (:cover-image        "COVER_IMAGE"         nil nil nil)
    (:cover-image-alt    "COVER_IMAGE_ALT"     nil nil nil)
    (:visibility         "VISIBILITY"          nil nil nil)
    (:status             "STATUS"              nil nil nil)
    (:theme              "THEME"               nil nil nil)
    (:destination-folder "DESTINATION_FOLDER"  nil nil nil)
    (:destination-folder "DESTINATION-FOLDER"  nil nil nil)
    (:astro-publish-date "ASTRO_PUBLISH_DATE"  nil nil nil)
    (:astro-excerpt      "ASTRO_EXCERPT"       nil nil nil)
    (:astro-image        "ASTRO_IMAGE"         nil nil nil)
    (:astro-image-alt    "ASTRO_IMAGE_ALT"     nil nil nil)
    (:astro-author-image "ASTRO_AUTHOR_IMAGE"  nil nil nil)
    (:astro-tags         "ASTRO_TAGS"          nil nil 'newline)
    (:astro-date-occurred "ASTRO_DATE_OCCURRED" nil nil nil)
    (:astro-categories   "ASTRO_CATEGORIES"    nil nil 'newline)
    (:place              "PLACE"               nil nil nil)
    (:astro-place        "ASTRO_PLACE"         nil nil nil)
    (:era                "ERA"                 nil nil nil)
    (:astro-era          "ASTRO_ERA"           nil nil nil)
    (:places             "PLACES"              nil nil 'newline)
    (:astro-places       "ASTRO_PLACES"        nil nil 'newline)
    (:themes             "THEMES"              nil nil 'newline)
    (:astro-themes       "ASTRO_THEMES"        nil nil 'newline)
    (:people             "PEOPLE"              nil nil 'newline)
    (:astro-people       "ASTRO_PEOPLE"        nil nil 'newline)
    (:emotions           "EMOTIONS"            nil nil 'newline)
    (:astro-emotions     "ASTRO_EMOTIONS"      nil nil 'newline)
    (:media              "MEDIA"               nil nil 'newline)
    (:astro-media        "ASTRO_MEDIA"         nil nil 'newline)
    (:story-type         "STORY_TYPE"          nil nil nil)
    (:astro-story-type   "ASTRO_STORY_TYPE"    nil nil nil)
    (:incomplete         "INCOMPLETE"          nil nil nil)
    (:astro-incomplete   "ASTRO_INCOMPLETE"    nil nil nil)
    (:connection-temporal "CONNECTION_TEMPORAL" nil nil 'newline)
    (:astro-connection-temporal "ASTRO_CONNECTION_TEMPORAL" nil nil 'newline)
    (:connection-emotional "CONNECTION_EMOTIONAL" nil nil 'newline)
    (:astro-connection-emotional "ASTRO_CONNECTION_EMOTIONAL" nil nil 'newline)
    (:connection-thematic "CONNECTION_THEMATIC" nil nil 'newline)
    (:astro-connection-thematic "ASTRO_CONNECTION_THEMATIC" nil nil 'newline)
    (:astro-imports      "ASTRO_IMPORTS"       nil nil 'newline)
    (:astro-posts-folder "ASTRO_POSTS_FOLDER"  nil nil nil)
    (:astro-date-format  nil "date-format" org-astro-date-format nil)))

(provide 'ox-astro)

;;; ox-astro.el ends here
