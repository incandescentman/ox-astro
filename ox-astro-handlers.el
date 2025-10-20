;;; ox-astro-handlers.el --- Filter and handler functions for ox-astro  -*- lexical-binding: t -*-

;;; Commentary:
;; Filter and handler functions for ox-astro export

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-astro-config)
(require 'cl-lib)

;; Declare functions from other ox-astro modules
(declare-function org-astro--collect-images-from-tree "ox-astro-image-handlers")
(declare-function org-astro--build-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--build-render-map "ox-astro-image-handlers")
(declare-function org-astro--process-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--persist-wrap-raw-image-lines "ox-astro-image-handlers")
(declare-function org-astro--get-assets-folder "ox-astro-helpers")
(declare-function org-astro--sanitize-filename "ox-astro-helpers")
(declare-function org-astro--path-to-var-name "ox-astro-helpers")
(declare-function org-astro--get-front-matter-data "ox-astro-helpers")
(declare-function org-astro--gen-yaml-front-matter "ox-astro-helpers")
(declare-function org-astro--dbg-log "ox-astro-helpers")
(declare-function org-astro--slugify "ox-astro-helpers")
(declare-function org-astro--get-title "ox-astro-helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variable to persist data across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

(defun org-astro-auto-wrap-image-paths-filter (tree _backend info)
  "Pre-processing filter that automatically wraps raw image paths in [[ ]] brackets.
This runs FIRST, before all other processing, to simulate manual bracket addition."
  ;; Just wrap raw paths - keep it simple
  (let ((src-file (or (plist-get info :input-file)
                      (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
    (when src-file
      ;; Step 1: Wrap raw paths in brackets and save
      (let ((count (org-astro--persist-wrap-raw-image-lines src-file)))
        (when (> count 0)
          (message "Auto-wrapped %d raw image paths in source file" count)))))

  ;; Return tree unchanged
  tree)



(defun org-astro-prepare-images-filter (tree _backend info)
  "Ensure image import metadata is present on INFO for later phases."
  (org-astro--dbg-log info "=== Starting prepare-images-filter ===")
  ;; Reset transient state so we always repopulate from the most recent context.
  (setq org-astro--current-body-images-imports nil)
  (plist-put info :astro-body-images-imports nil)
  (plist-put info :astro-uses-linkpeek nil)

  (let* ((context (plist-get info :astro-export-context))
         (_ (when context
              (let ((manifest (plist-get context :manifest)))
                (when manifest
                  (setq info (cl-putf info :astro-image-manifest manifest))))))
         (processed (plist-get context :processed)))
    (unless processed
      (let* ((posts-folder-raw (or (plist-get info :destination-folder)
                                   (plist-get info :astro-posts-folder)))
             (folder-config (and posts-folder-raw
                                 (cdr (assoc posts-folder-raw org-astro-known-posts-folders))))
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
                            (t nil)))
             (manifest (or (plist-get info :astro-image-manifest)
                           (org-astro--build-image-manifest tree info)))
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
             (result (when posts-folder
                       (org-astro--process-image-manifest manifest posts-folder sub-dir)))
             (processed-result (plist-get result :entries))
             (refreshed-context (list :manifest manifest
                                      :processed processed-result
                                      :posts-folder posts-folder
                                      :sub-dir sub-dir)))
        (when manifest
          (setq info (cl-putf info :astro-image-manifest manifest)))
        (setq info (cl-putf info :astro-export-context refreshed-context))
        (setq processed processed-result)))

    ;; Persist the processed list (from either preflight or fallback work).
    (when processed
      (plist-put info :astro-body-images-imports processed)
      (setq org-astro--current-body-images-imports processed))
    ;; Build render metadata for downstream consumers.
    (let* ((render-data (and processed (org-astro--build-render-map processed)))
           (render-map (and render-data (plist-get render-data :map)))
           (render-imports (and render-data (plist-get render-data :imports))))
      (plist-put info :astro-render-map render-map)
      (plist-put info :astro-render-imports render-imports)))
  tree)



(defun org-astro-body-filter (body _backend info)
  "Add front-matter, source comment, and imports to BODY."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--get-front-matter-data tree info))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         ;; Copy frontmatter to clipboard
         (_ (let ((pbcopy (executable-find "pbcopy")))
              (when (and pbcopy front-matter-string)
                (condition-case _
                    (with-temp-buffer
                      (insert front-matter-string)
                      (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                  (error nil)))))
         ;; Add an HTML comment noting the source .org file path, placed
         ;; after the frontmatter (frontmatter should remain at top-of-file).
         (source-path (or (plist-get info :input-file)
                          (and (buffer-file-name) (expand-file-name (buffer-file-name)))))
         (source-comment (when source-path
                           (format "{/* Source org: %s */}\n" source-path)))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (render-imports (plist-get info :astro-render-imports))
         (render-imports-string (when render-imports
                                  (mapconcat #'identity render-imports "\n")))
         ;; 2. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 3. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (plist-get info :astro-uses-linkpeek)
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 4. ImageGallery component import (if GALLERY blocks are used)
         (has-gallery-blocks (org-element-map tree 'special-block
                               (lambda (block)
                                 (string-equal (org-element-property :type block) "GALLERY"))
                               nil t))
         (image-gallery-import (when has-gallery-blocks
                                 "import ImageGallery from '../../components/ImageGallery.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list render-imports-string linkpeek-import image-gallery-import manual-imports))
                                 "\n")))
    (concat front-matter-string
            (or source-comment "")
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
         (entity-map '(("&#x2013;" . "–")
                       ("&rsquo;" . "'")
                       ("&lsquo;" . "'")
                       ("&rdquo;" . "\"")
                       ("&ldquo;" . "\""))))
    ;; Replace HTML entities
    (dolist (pair entity-map)
      (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
    ;; Normalize self-closing tags so MDX doesn't expect separate closing tags.
    (let ((case-fold-search t))
      (setq s (replace-regexp-in-string "<br\\s*/?>" "<br />" s)))
    ;; Convert markdown image syntax with absolute paths to Image components
    (let ((pattern "!\\[\\([^]]*\\)\\](\\([~/][^)]+\\.\\(?:png\\|jpe?g\\|webp\\)\\))"))
      (setq s (replace-regexp-in-string
               pattern
               (lambda (match)
                 (let* ((alt (match-string 1 match))
                        (path (match-string 2 match))
                        (record (org-astro--lookup-render-record path info)))
                   (if record
                       (let ((var-name (plist-get record :var-name)))
                         (if (and var-name (not (string-blank-p alt)))
                             (org-astro--format-image-component var-name alt)
                           (plist-get record :jsx)))
                     match)))
               s t t)))
    ;; Indented blocks to blockquotes (but skip JSX components)
    (let* ((lines (split-string s "\n"))
           (in-jsx-component nil)
           (processed-lines
            (mapcar (lambda (line)
                      ;; Track if we're inside a JSX component
                      (cond
                       ;; Opening JSX component tag (like <ImageGallery)
                       ((string-match-p "^\\s-*<[A-Z]" line)
                        (setq in-jsx-component t)
                        line)
                       ;; Self-closing tag end (like />)
                       ((and in-jsx-component (string-match-p "/>\\s-*$" line))
                        (setq in-jsx-component nil)
                        line)
                       ;; JSX component closing tag (like >)
                       ((and in-jsx-component (string-match-p "^\\s-*>\\s-*$" line))
                        (setq in-jsx-component nil)
                        line)
                       ;; Don't convert indented lines inside JSX components
                       ((and in-jsx-component (string-prefix-p "    " line))
                        line)
                       ;; Convert regular indented lines to blockquotes
                       ((string-prefix-p "    " line)
                        (concat "> " (substring line 4)))
                       (t line)))
                    lines)))
      (setq s (mapconcat 'identity processed-lines "\n")))
    ;; Final pass to ensure any <br> variants are self-closing.
    (let ((case-fold-search t))
      (setq s (replace-regexp-in-string "<br\\s*/?>" "<br />" s)))
    s))

(provide 'ox-astro-handlers)
