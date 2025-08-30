;;; test-underscore.el -- Test underscore image processing
(setq load-path (cons "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro" load-path))
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-config.el")
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-helpers.el")
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-handlers.el") 
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro.el")

;; Override the interactive prompt by setting the destination folder directly
(defvar org-astro-known-posts-folders 
  '(("jaydocs" . "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog")))

;; Test with the massimo-dutti file
(find-file "/Users/jay/Library/CloudStorage/Dropbox/roam/consumerist/20250827235900-massimo_dutti.org")

(message "Starting ox-astro export test...")

;; Disable interactive prompts by binding completing-read
(cl-letf (((symbol-function 'completing-read) 
           (lambda (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
             "jaydocs")))
  (org-astro-export-to-mdx))

(message "Export completed!")