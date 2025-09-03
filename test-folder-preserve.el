;;; Test script for destination-specific folder structure preservation

;; Load ox-astro
(add-to-list 'load-path "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro")
(require 'ox-astro)

;; Configure source root folder
(setq org-astro-source-root-folder
      "/Users/jay/Library/CloudStorage/Dropbox/roam")

;; Configure known posts folders with new plist format
;; astro-roam has :preserve-folder-structure t
;; others do not preserve folder structure
(setq org-astro-known-posts-folders
      '(("blog" . (:path "~/projects/my-astro-site/src/content/blog"))
        ("docs" . (:path "~/projects/my-docs-site/src/content/docs"))
        ("jaydocs" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog"))
        ("astro-roam" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-roam/src/content"
                         :preserve-folder-structure t))))

;; Test function to show where a file would be exported
(defun test-export-location (org-file destination-nickname)
  "Show where ORG-FILE would be exported to DESTINATION-NICKNAME."
  (let* ((source-file (expand-file-name org-file))
         (folder-config (cdr (assoc destination-nickname org-astro-known-posts-folders)))
         (dest-path (if (stringp folder-config)
                       folder-config
                     (plist-get folder-config :path)))
         (preserve-structure (and (listp folder-config)
                                  (plist-get folder-config :preserve-folder-structure)))
         (source-root (expand-file-name org-astro-source-root-folder))
         (relative-path (when (string-prefix-p source-root source-file)
                         (file-relative-name source-file source-root)))
         (preserved-subdir (when (and preserve-structure relative-path)
                            (let ((dir (file-name-directory relative-path)))
                              (when (and dir (not (string= dir "./")))
                                dir)))))
    (message "==== Export Location Test ====")
    (message "Source file: %s" org-file)
    (message "Destination: %s" destination-nickname)
    (message "Preserve structure: %s" (if preserve-structure "YES" "NO"))
    (when preserve-structure
      (message "Relative path from root: %s" relative-path)
      (message "Preserved subdirectory: %s" (or preserved-subdir "(none)")))
    (message "Output location: %s%s"
             (expand-file-name dest-path)
             (or preserved-subdir ""))))

;; Test examples
(message "Testing folder preservation for different destinations:")
(message "")

;; Test with astro-roam (SHOULD preserve structure)
(test-export-location "/Users/jay/Library/CloudStorage/Dropbox/roam/journal/2025-09-03.org" "astro-roam")
(message "")

;; Test with jaydocs (should NOT preserve structure)  
(test-export-location "/Users/jay/Library/CloudStorage/Dropbox/roam/journal/2025-09-03.org" "jaydocs")
(message "")

(message "Run M-x org-astro-export-to-mdx in an Org file to actually export with these settings")