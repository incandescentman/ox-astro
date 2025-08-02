;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filter Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-prepare-images-filter (tree _backend info)
  "Find all local images, process them, and store import data in INFO.
This filter runs on the parse TREE before transcoding. It collects
all local image links, copies them to the Astro assets
directory, and prepares a list of import statements to be added
to the final MDX file. The data is stored in the INFO plist
under the key `:astro-body-images-imports`."
  (let* ((posts-folder (or (plist-get info :posts-folder)
                           (plist-get info :astro-posts-folder)))
         ;; Collect all image links from the document body.
         (image-paths (org-astro--collect-images-from-tree tree))
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
        (plist-put info :astro-body-images-imports final-data))))
  ;; Return the tree, as required for a parse-tree filter.
  tree)

(defun org-astro-body-filter (body _backend info)
  "Add front-matter and imports to the BODY of the document."
  (let* ((tree (plist-get info :parse-tree))  ; Use the already-parsed tree from export
         (front-matter-data (org-astro--get-front-matter-data tree info))
         (front-matter-string (org-astro--gen-yaml-front-matter front-matter-data))
         ;; --- Handle All Imports ---
         ;; 1. Body image imports (collected by our filter)
         (body-images-imports (plist-get info :astro-body-images-imports))
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
         (linkpeek-import (when (or (plist-get info :astro-uses-linkpeek)
                                    (string-match-p "\\[\\(https?://[^]]+\\)\\](\\1)" body))
                            "import LinkPeek from '../../components/ui/LinkPeek.astro';"))
         ;; 5. Combine all imports, filtering out nil/empty values
         (all-imports (mapconcat #'identity
                                 (delq nil (list astro-image-import linkpeek-import body-imports-string manual-imports))
                                 "\n")))
    (concat front-matter-string
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
         ;; HTML entities
         (s (replace-regexp-in-string "&#x2013;" "–" s t t))
         (s (replace-regexp-in-string "&rsquo;" "'" s t t))
         (s (replace-regexp-in-string "&lsquo;" "'" s t t))
         (s (replace-regexp-in-string "&rdquo;" "\"" s t t))
         (s (replace-regexp-in-string "&ldquo;" "\"" s t t))
         ;; Convert markdown image syntax with absolute paths to Image components
         (image-imports (plist-get info :astro-body-images-imports))
         (s (if image-imports
                (replace-regexp-in-string
                 "!\\[\\([^]]*\\)\\](\\(/[^)]+\\.\\(?:png\\|jpe?g\\)\\))"
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
                 s)
                s))
         ;; Convert markdown links that are raw URLs to LinkPeek components
         (s (replace-regexp-in-string
             "\\[\\(https?://[^]]+\\)\\](\\(\\1\\))"
             (lambda (match)
               (let ((url (match-string 1 match)))
                 ;; Mark that we're using LinkPeek (for import)
                 (plist-put info :astro-uses-linkpeek t)
                 (format "<LinkPeek href=\"%s\"></LinkPeek>" url)))
             s))
         ;; Indented blocks to blockquotes
         (lines (split-string s "\n"))
         (processed-lines
          (mapcar (lambda (line)
                    (if (string-prefix-p "    " line)
                        (concat "> " (substring line 4))
                        line))
                  lines))
         (s (mapconcat 'identity processed-lines "\n")))
    s))

(provide 'ox-astro-handlers)
