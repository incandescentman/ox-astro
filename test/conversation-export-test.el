(require 'ert)
(require 'org)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'ox-astro)

(defun ox-astro-test--count-regexp (regexp text)
  "Count non-overlapping matches for REGEXP in TEXT."
  (let ((count 0)
        (start 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(ert-deftest org-astro-conversation-blocks-survive-hostile-babel-defaults ()
  "Conversation blocks should export source text even if Babel defaults use results."
  (let* ((temp-project (make-temp-file "ox-astro-conversation-test" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (org-file (expand-file-name "conversation.org" temp-project))
         (org-content
          "#+TITLE: Conversation Export Regression\n#+PUBLISH_DATE: [2026-02-16 Mon 14:00]\n#+SUBHED: Regression fixture\n#+SLUG: conversation-export-regression\n#+DESTINATION_FOLDER: test\n\n#+begin_src user\nfirst user prompt\n#+end_src\n\n#+begin_src assistant\nassistant reply\n#+end_src\n\n#+begin_src verse\nline one\nline two\n#+end_src\n")
         (org-astro-known-posts-folders `(("test" . (:path ,posts-dir))))
         (org-astro-source-root-folder temp-project)
         (org-export-show-temporary-export-buffer nil)
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (org-confirm-babel-evaluate nil)
         (mdx nil))
    (make-directory posts-dir t)
    (with-temp-file org-file
      (insert org-content))
    (let ((buffer (find-file-noselect org-file))
          ;; Simulate hostile defaults from user/global config.
          (org-babel-default-header-args:user '((:exports . "results")))
          (org-babel-default-header-args:assistant '((:exports . "results")))
          (org-babel-default-header-args:verse '((:exports . "results"))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (let ((org-astro-debug-images nil)
                    (org-astro-debug-console nil)
                    (org-astro-debug-log nil))
                (org-astro-export-to-mdx)))
            (let ((mdx-path (expand-file-name "conversation-export-regression.mdx" posts-dir)))
              (setq mdx (with-temp-buffer
                          (insert-file-contents mdx-path)
                          (buffer-string)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    (when (file-directory-p temp-project)
      (delete-directory temp-project t))
    (should (string-match-p "```user\nfirst user prompt\n```" mdx))
    (should (string-match-p "```assistant\nassistant reply\n```" mdx))
    (should (string-match-p "```verse\nline one\nline two\n```" mdx))
    (should (= 1 (ox-astro-test--count-regexp "```user" mdx)))
    (should (= 1 (ox-astro-test--count-regexp "```assistant" mdx)))
    (should (= 1 (ox-astro-test--count-regexp "```verse" mdx)))
    (should (= 0 (ox-astro-test--count-regexp "```user\\s-*\\n\\s-*\\n```" mdx)))
    (should (= 0 (ox-astro-test--count-regexp "```assistant\\s-*\\n\\s-*\\n```" mdx)))
    (should (= 0 (ox-astro-test--count-regexp "```verse\\s-*\\n\\s-*\\n```" mdx)))))

(provide 'conversation-export-test)
