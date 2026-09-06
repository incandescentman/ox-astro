(require 'ert)
(require 'org)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'ox-astro)

(defun ox-astro-test--excerpt-from-string (content)
  "Return the Astro excerpt derived from Org CONTENT."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (info (org-export-get-environment 'astro)))
      (plist-put info :parse-tree tree)
      (org-astro--get-excerpt tree info))))

(ert-deftest org-astro-excerpt-skips-headline-before-prose ()
  "A leading headline must not displace the first body prose paragraph."
  (let* ((prose "This is the first real prose paragraph, and it continues long enough to require careful truncation at a word boundary without cutting the final visible word into pieces for the blog card description.")
         (excerpt (ox-astro-test--excerpt-from-string
                   (concat "#+TITLE: Test\n\n* Heading\n" prose "\n"))))
    (should (string-prefix-p "This is the first real prose paragraph" excerpt))
    (should (string-suffix-p "…" excerpt))
    (should (<= (length excerpt) 160))
    (should-not (string-match-p "piec…\\'" excerpt))))

(ert-deftest org-astro-excerpt-omits-link-list-only-body ()
  "A body containing only a link list has no fallback excerpt."
  (let ((excerpt (ox-astro-test--excerpt-from-string
                  "#+TITLE: Links\n\n* Links\n- [[https://example.com/a][Alpha]]\n- [[https://example.com/b][Beta]]\n")))
    (should-not excerpt)
    (should-not (string-match-p
                 "^excerpt:"
                 (org-astro--gen-yaml-front-matter
                  `((title . "Links") (excerpt . ,excerpt)))))))

(ert-deftest org-astro-excerpt-preserves-authored-description ()
  "An authored description wins over any fallback paragraph."
  (let ((description "The authored description stays in charge exactly as supplied, even when it is longer than the automatic fallback limit and would have been truncated if it came from the body instead of an explicit keyword."))
    (should (equal
             description
             (ox-astro-test--excerpt-from-string
              (format "#+TITLE: Authored\n#+DESCRIPTION: %s\n\n* Heading\nDifferent body prose.\n"
                      description))))))

(ert-deftest org-astro-excerpt-uses-chat-fragment-prose ()
  "A prose paragraph after a chat source block is still valid fallback text."
  (should (equal
           "Exactly — this is real prose even though it begins a chat reply."
           (ox-astro-test--excerpt-from-string
            "#+TITLE: Chat\n\n* Chat\n#+begin_src user\nTell me something.\n#+end_src\n\nExactly — this is real prose even though it begins a chat reply.\n"))))

(ert-deftest org-astro-excerpt-skips-non-prose-structures ()
  "Drawers, images, tables, source blocks, and example blocks are not excerpts."
  (should (equal
           "The first actual prose paragraph wins."
           (ox-astro-test--excerpt-from-string
            ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Structures\n\n* Heading\n[[file:/tmp/image.jpg]]\n\n| A | B |\n| 1 | 2 |\n\n#+begin_src text\nSource text.\n#+end_src\n\n#+begin_example\nExample text.\n#+end_example\n\nThe first actual prose paragraph wins.\n"))))

(provide 'excerpt-fallback-test)
