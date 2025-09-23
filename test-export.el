;; Test script for slug generation
(add-to-list 'load-path "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro")

;; Load the ox-astro package
(require 'ox-astro-config)
(require 'ox-astro-helpers)
(require 'ox-astro-handlers)
(require 'ox-astro-table-handlers)
(require 'ox-astro-image-handlers)
(require 'ox-astro-pdf-handlers)
(require 'ox-astro)

;; Open the test file
(find-file "test-slug-generation.org")

;; Export to MDX
(message "Exporting test file...")
(org-astro-export-to-mdx)

(message "Export complete. Check if SLUG was added to the file.")