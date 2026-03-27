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
    (should (string-match-p
             (regexp-quote "{/* theme: claude */}\n\n<div class=\"model-banner\">Claude Sonnet 4.5</div>\n\n# Claude")
             mdx))
    ;; Theme marker should only appear once
    (let ((count 0) (start 0)
          (marker (regexp-quote "{/* theme: claude */}")))
      (while (string-match marker mdx start)
        (setq count (1+ count))
        (setq start (match-end 0)))
      (should (= 1 count)))))

(ert-deftest org-astro-empty-theme-section-does-not-duplicate-model-banner ()
  "An empty themed heading should hoist its MODEL banner only once."
  (let* ((content
          "#+TITLE: Should Vinh move?\n#+SLUG: should-vinh-move\n#+DESTINATION_FOLDER: test\n\n[[/tmp/a.png]]\n* Claude\n#+THEME: claude\n#+MODEL: Claude Sonnet 4.5\n\n* Should Vinh move?\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "empty-theme-section.org" "should-vinh-move")))
    (should (string-match-p
             (regexp-quote "![img](/tmp/a.png)  \n\n{/* theme: claude */}\n\n<div class=\"model-banner\">Claude Sonnet 4.5</div>\n\n# Claude\n\n# Should Vinh move?")
             mdx))
    (let ((count 0)
          (start 0)
          (banner (regexp-quote "<div class=\"model-banner\">Claude Sonnet 4.5</div>")))
      (while (string-match banner mdx start)
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

(ert-deftest org-astro-doc-level-model-banner-and-no-dup-theme ()
  "Document-level MODEL emits a banner and theme markers are not duplicated."
  (let* ((content "#+TITLE: Multi AI\n#+DESTINATION_FOLDER: test\n\n#+THEME: chatgpt\n#+MODEL: ChatGPT 5.2\n\n* Intro\nBody text.\n\n#+THEME: claude\n\n* Claude\n#+MODEL: Claude Sonnet 4.5\nBody.\n")
         (mdx (ox-astro-test--with-temp-export content "doc-model.org" "multi-ai")))
    (should (string-match-p
             (regexp-quote "{/* theme: chatgpt */}\n\n<div class=\"model-banner\">ChatGPT 5.2</div>\n\n# Intro")
             mdx))
    (let ((count 0) (start 0)
          (marker (regexp-quote "{/* theme: claude */}")))
      (while (string-match marker mdx start)
        (setq count (1+ count))
        (setq start (match-end 0)))
      (should (= 1 count)))))

(ert-deftest org-astro-normalizes-inline-reset-theme-aliases ()
  "Reset aliases should export as the canonical default theme marker."
  (let* ((content "#+TITLE: Notes\n#+DESTINATION_FOLDER: test\n\n* Claude\n#+THEME: claude\nBody text.\n\n* Transcript\n#+THEME: human\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "reset-alias-inline.org" "notes")))
    (should (string-match-p (regexp-quote "{/* theme: claude */}\n\n# Claude") mdx))
    (should (string-match-p (regexp-quote "{/* theme: default */}\n\n# Transcript") mdx))
    (should-not (string-match-p "{/\\* theme: human \\*/}" mdx))))

(ert-deftest org-astro-normalizes-doc-level-reset-theme-aliases ()
  "Document-level reset aliases should normalize to theme: default."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n#+THEME: jay\n* Notes\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "reset-alias-doc.org" "notes")))
    (should (string-match-p "theme: default" mdx))
    (should-not (string-match-p "theme: jay" mdx))))

(provide 'theme-reorder-test)
