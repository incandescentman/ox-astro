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
  "Find all local images, process them, and store import data in INFO.
This filter runs on the parse TREE before transcoding. It collects
all local image links, copies them to the Astro assets
directory, and prepares a list of import statements to be added
to the final MDX file. The data is stored in the INFO plist
under the key `:astro-body-images-imports`.

NOTE: If org-astro--current-body-images-imports is already set,
preprocessing has already been completed and we skip the processing."
  ;; Reset any stale state from previous exports so we never carry images over.
  (setq org-astro--current-body-images-imports nil)
  (plist-put info :astro-body-images-imports nil)
  ;; Also reset LinkPeek flag so imports don't leak across exports
  (plist-put info :astro-uses-linkpeek nil)
  (let* ((posts-folder-raw (or (plist-get info :destination-folder)
                               (plist-get info :astro-posts-folder)))
         ;; Resolve the posts folder using the same logic as in ox-astro.el
         (resolved-posts-folder-raw (and posts-folder-raw
                                         (cdr (assoc posts-folder-raw org-astro-known-posts-folders))))
         ;; Trim whitespace from resolved path to handle configuration errors
         (resolved-posts-folder (and resolved-posts-folder-raw
                                     (string-trim resolved-posts-folder-raw)))
         (posts-folder (cond
                        ;; If we found it in known folders, use that path
                        (resolved-posts-folder resolved-posts-folder)
                        ;; If posts-folder-raw exists and looks like an absolute path, use it directly
                        ((and posts-folder-raw 
                              (file-name-absolute-p posts-folder-raw)
                              (file-directory-p (expand-file-name posts-folder-raw)))
                         posts-folder-raw)
                        ;; Otherwise, can't resolve - no posts folder
                        (t nil)))
         ;; Collect all image links from the document body using multiple strategies.
         (image-paths-from-tree (org-astro--collect-images-from-tree tree))
         ;; Also collect from raw buffer content to catch underscore paths and fix subtree issues
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
                     ;; Only auto-generate slug if title came from headline (not from #+TITLE keyword)
                     (when title-from-headline
                       (org-astro--slugify title)))))
         (sub-dir (if slug (concat "posts/" slug "/") "posts/"))
         image-imports-data)
    (when posts-folder
      (message "DEBUG: Processing %d images in posts folder: %s" (length image-paths) posts-folder)
      (dolist (path image-paths)
        (message "DEBUG: Processing image path: %s" path)
        ;; For each image, copy it to assets and get its new path.
        (let* ((astro-path (org-astro--process-image-path path posts-folder sub-dir t))
               (var-name (org-astro--path-to-var-name path))
               (clean-filename (org-astro--sanitize-filename (file-name-nondirectory path)))
               (target-abs (when astro-path
                             (expand-file-name clean-filename (org-astro--get-assets-folder posts-folder sub-dir)))))
          (message "DEBUG: Astro path: %s, var name: %s" astro-path var-name)
          (when (and astro-path var-name)
            (push `(:path ,path :var-name ,var-name :astro-path ,astro-path :target-path ,target-abs)
                  image-imports-data))))
      ;; Note: Source buffer saving is handled by org-astro--update-source-buffer-image-path
      (when image-imports-data
        (message "DEBUG: Processed %d images for import" (length image-imports-data))
        ;; Insert or update a suggestions block with new image paths in the source org buffer
        (ignore-errors
          (let ((src (or (plist-get info :input-file)
                         (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
            (when src
              (org-astro--upsert-image-paths-comment-into-file src image-imports-data))))
        ))
    ;; Store the collected data in the info plist for other functions to use.
    (when image-imports-data
      (let ((final-data (nreverse image-imports-data)))
        ;; Store in both places for data persistence across export phases
        (setq org-astro--current-body-images-imports final-data)
        (plist-put info :astro-body-images-imports final-data)))
    
    ;; CRITICAL: Force re-parse of the tree with the updated content
    (let ((src-file (or (plist-get info :input-file)
                        (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
      (when (and src-file image-imports-data)
        (message "Forcing buffer reload and re-parse with updated image paths")
        ;; Force reload and return a fresh parse tree
        (revert-buffer t t)
        (setq tree (org-element-parse-buffer)))))
  ;; Return the potentially updated tree
  tree)

(defun org-astro-body-filter (body _backend info)
  "Add front-matter, source comment, and imports to BODY."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--get-front-matter-data tree info))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         ;; Add an HTML comment noting the source .org file path, placed
         ;; after the frontmatter (frontmatter should remain at top-of-file).
         (source-path (or (plist-get info :input-file)
                          (and (buffer-file-name) (expand-file-name (buffer-file-name)))))
         (source-comment (when source-path
                           (format "{/* Source org: %s */}\n" source-path)))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (body-images-imports-raw (or (plist-get info :astro-body-images-imports)
                                      org-astro--current-body-images-imports))
         ;; Check if first image is being used as hero (no explicit hero specified)
         (explicit-hero (or (plist-get info :astro-image)
                            (plist-get info :cover-image)))
         (body-images-imports (if (and (not explicit-hero) body-images-imports-raw)
                                  ;; Exclude first image when it's used as hero
                                  (cdr body-images-imports-raw)
                                ;; Use all images when there's an explicit hero
                                body-images-imports-raw))
         (body-imports-string
          (when body-images-imports
            (mapconcat
             (lambda (item)
               (format "import %s from '%s';"
                       (plist-get item :var-name)
                       (plist-get item :astro-path)))
             body-images-imports
             "\n")))
         ;; 2. Hero image import (if first body image is being used as hero)
         (posts-folder (or (plist-get info :destination-folder)
                           (plist-get info :astro-posts-folder)))
         (hero-import (when (and (not explicit-hero) body-images-imports-raw posts-folder)
                        (let ((first-image (car body-images-imports-raw)))
                          (format "import hero from '%s';"
                                  (plist-get first-image :astro-path)))))
         ;; 3. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 4. Astro Image component import (always include if we have any body images)
         (astro-image-import (when body-images-imports
                               "import { Image } from 'astro:assets';"))
         ;; 5. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (plist-get info :astro-uses-linkpeek)
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 6. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list hero-import astro-image-import linkpeek-import body-imports-string manual-imports))
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
    ;; Convert markdown image syntax with absolute paths to Image components
    (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                  org-astro--current-body-images-imports))
           ;; Exclude first image when it's being used as hero (same logic as imports)
           (explicit-hero (or (plist-get info :astro-image)
                              (plist-get info :cover-image)))
           (image-imports (if (and (not explicit-hero) image-imports-raw)
                              ;; Exclude first image when it's used as hero
                              (cdr image-imports-raw)
                            ;; Use all images when there's an explicit hero
                            image-imports-raw)))
      (when image-imports
        (setq s (replace-regexp-in-string
                 "!\[\([^]]*\)\](\(/[^)]+\.\(?:png\|jpe?g\|webp\)\))"
                 (lambda (match)
                   (let* ((alt (match-string 1 match))
                          (path (match-string 2 match))
                          (image-data (cl-find path image-imports
                                               :key (lambda (item) (plist-get item :path))
                                               :test #'string-equal)))
                     (if image-data
                         (let ((var-name (plist-get image-data :var-name))
                               (alt-text (or (org-astro--filename-to-alt-text path) alt "Image")))
                           (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                       match)))
                 s))))
    ;; Indented blocks to blockquotes
    (let* ((lines (split-string s "\n"))
           (processed-lines
            (mapcar (lambda (line)
                      (if (string-prefix-p "    " line)
                          (concat "> " (substring line 4))
                        line))
                    lines)))
      (setq s (mapconcat 'identity processed-lines "\n")))
    s))

(provide 'ox-astro-handlers)
