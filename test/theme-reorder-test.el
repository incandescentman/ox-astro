(require 'ert)
(require 'org)
(require 'subr-x)
(require 'cl-lib)

;; Ensure test directory is on the load-path so we can reuse helpers.
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-dir))

;; Reuse helper and load-path setup from date-title-and-subtree-test
(require 'date-title-and-subtree-test)

(ert-deftest org-astro-hoists-theme-keyword-before-heading ()
  "If a heading is immediately followed by #+THEME:, emit the theme before the heading."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n* Claude\n#+THEME: claude\n#+MODEL: Claude Sonnet 4.5\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "misordered-theme.org" "claude")))
    ;; Theme marker should appear before the heading
    (should (string-match-p (regexp-quote "{/* theme: claude */}\n\n# Claude") mdx))
    ;; Theme marker should only appear once
    (let ((count 0) (start 0)
          (marker (regexp-quote "{/* theme: claude */}")))
      (while (string-match marker mdx start)
        (setq count (1+ count))
        (setq start (match-end 0)))
      (should (= 1 count)))))

(ert-deftest org-astro-respects-already-prefixed-theme ()
  "When THEME appears before the first heading, treat it as page theme (no inline marker)."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n#+THEME: claude\n* Claude\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "prefixed-theme.org" "claude")))
    ;; Page theme promoted to frontmatter, no inline marker emitted.
    (should (string-match-p "theme: claude" mdx))
    (should-not (string-match-p "{/* theme: claude */}" mdx))))

(provide 'theme-reorder-test)
