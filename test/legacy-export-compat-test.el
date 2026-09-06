(require 'ert)
(require 'org)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'ox-astro)

(defconst ox-astro-legacy-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest org-astro-legacy-list-continuation-exports-as-blockquote ()
  "Legacy absolute-destination posts retain quoted list continuations."
  (let* ((org-export-with-toc nil)
         (org-astro--legacy-export-active t)
         (source "#+TITLE: Legacy quote\n#+DESTINATION_FOLDER: /tmp/legacy-posts\n\n* Legacy quote\n- Label\n  Quoted continuation.\n")
         (output (org-export-string-as source 'astro t)))
    (should (string-match-p "-   Label" output))
    (should (string-match-p "\n> Quoted continuation\\." output))))

(ert-deftest org-astro-narrowed-export-restores-heading-and-hero-imports ()
  "A narrowed post keeps its root level and legacy hero import prolog."
  (let* ((temp-project (make-temp-file "ox-astro-legacy-subtree" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (source-file (expand-file-name "journal.txt" temp-project))
         (image-file
          (expand-file-name
           "test-files/image-rendering/images/local-photo.png"
           ox-astro-legacy-test--repo-root))
         (org-astro-known-posts-folders `(("test" . (:path ,posts-dir))))
         (org-astro-source-root-folder temp-project)
         output)
    (make-directory posts-dir t)
    (with-temp-file source-file
      (insert (format "* Journal\n** Child Post\n#+TITLE: Child Post\n#+SLUG: child-post\n#+DESTINATION_FOLDER: test\n#+PUBLISH_DATE: [2026-09-06 Sun 00:00]\n#+EXCERPT: Child excerpt\n\n[[%s]]\n*** Section\nBody.\n"
                      image-file)))
    (let ((buffer (find-file-noselect source-file)))
      (with-current-buffer buffer
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Child Post$")
        (org-back-to-heading t)
        (org-narrow-to-subtree)
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-astro-debug-images nil)
              (org-astro-copy-to-clipboard nil))
          (org-astro-export-to-mdx)))
      (kill-buffer buffer))
    (setq output
          (with-temp-buffer
            (insert-file-contents (expand-file-name "child-post.mdx" posts-dir))
            (buffer-string)))
    (should (string-match-p "^# Child Post$" output))
    (should (string-match-p "^## Section$" output))
    (should (string-match-p "^import hero from '~/assets/images/posts/child-post/local-photo.png';$" output))
    (should (string-match-p "^import { Image } from 'astro:assets';$" output))))

(ert-deftest org-astro-roam-source-preamble-keeps-populated-link ()
  "A populated org-roam Source item exports its link without boilerplate."
  (let ((mdx (org-export-string-as
              "- Source :: [[https://example.com/article][An article]]\n\n* Post\nBody.\n"
              'astro t)))
    (should (string-match-p "^\\[An article\\](https://example.com/article)$" mdx))
    (should-not (string-match-p "Source:" mdx))))

(ert-deftest org-astro-legacy-conversation-heading-resets-to-h1 ()
  "A legacy THEME/MODEL conversation section resets its heading hierarchy."
  (let ((mdx (org-export-string-as
              "* Healthy\n** Claude\n#+THEME: claude\n#+MODEL: Claude\n**** Books\nBody.\n"
              'astro t)))
    (should (string-match-p "^# Claude$" mdx))
    (should (string-match-p "^### Books$" mdx))
    (should-not (string-match-p "^## Claude$" mdx))))

(provide 'legacy-export-compat-test)
