;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global variable to persist data across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

(defun org-astro-prepare-images-filter (tree _backend info)
  "Find all local images, process them, and store import data in INFO.
This filter runs on the parse TREE before transcoding. It collects
all local image links, copies them to the Astro assets
directory, and prepares a list of import statements to be added
to the final MDX file. The data is stored in the INFO plist
under the key `:astro-body-images-imports`."
  (let* ((posts-folder (or (plist-get info :destination-folder)
                           (plist-get info :astro-posts-folder)))
         ;; Collect all image links from the document body.
         (image-paths-from-tree (org-astro--collect-images-from-tree tree))
         ;; Also collect from raw buffer content to catch underscore paths
         (image-paths-from-raw (org-astro--collect-raw-image-paths))
         (image-paths (delete-dups (append image-paths-from-tree image-paths-from-raw)))
         image-imports-data)
    (when posts-folder
      (dolist (path image-paths)
        ;; For each image, copy it to assets and get its new path.
        (let* ((astro-path (org-astro--process-image-path path posts-folder "posts/"))
               (var-name (org-astro--path-to-var-name path)))
          (when (and astro-path var-name)
            (push `(:path ,path :var-name ,var-name :astro-path ,astro-path)
                  image-imports-data)))))
    ;; Store the collected data in the info plist for other functions to use.
    (when image-imports-data
      (let ((final-data (nreverse image-imports-data)))
        ;; Store in both places for data persistence across export phases
        (setq org-astro--current-body-images-imports final-data)
        (plist-put info :astro-body-images-imports final-data))))
  ;; Return the tree, as required for a parse-tree filter.
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
         (body-images-imports (or (plist-get info :astro-body-images-imports)
                                  org-astro--current-body-images-imports))
         (body-imports-string
          (when body-images-imports
            (mapconcat
             (lambda (item)
               (format "import %s from '%s';"
                       (plist-get item :var-name)
                       (plist-get item :astro-path)))
             body-images-imports
             "\n")))
         ;; 2. Manual imports from #+ASTRO_IMPORTS
         (manual-imports (plist-get info :astro-imports))
         ;; 3. Astro Image component import (always include if we have any body images)
         (astro-image-import (when body-images-imports
                               "import { Image } from 'astro:assets';"))
         ;; 4. LinkPeek component import (if raw URLs are used - check body for raw URL patterns)
         (linkpeek-import (when (plist-get info :astro-uses-linkpeek)
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list astro-image-import linkpeek-import body-imports-string manual-imports))
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
                       ("&rdquo;" . \"\")
                       ("&ldquo;" . \"\"))))
    ;; Replace HTML entities
    (dolist (pair entity-map)
      (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t)))
    ;; Convert markdown image syntax with absolute paths to Image components
    (let ((image-imports (or (plist-get info :astro-body-images-imports)
                             org-astro--current-body-images-imports)))
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
