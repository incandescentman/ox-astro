(require 'ert)
(require 'org)
(require 'subr-x)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'ox-astro)

(defun ox-astro-test--with-temp-export (org-content org-filename expected-slug &optional narrow-heading point-adjust-fn)
  "Export ORG-CONTENT from ORG-FILENAME and return resulting MDX as string.
EXPECTED-SLUG must be the slug-based filename (without extension).
If NARROW-HEADING is non-nil, narrow to the subtree whose heading matches it.
When POINT-ADJUST-FN is provided, call it after narrowing to reposition point."
  (let* ((temp-project (make-temp-file "ox-astro-date-title" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (org-file (expand-file-name org-filename temp-project))
         (org-astro-known-posts-folders `(("test" . (:path ,posts-dir))))
         (org-astro-source-root-folder temp-project)
         (org-export-show-temporary-export-buffer nil)
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil))
    (make-directory posts-dir t)
    (with-temp-file org-file
      (insert org-content))
    (let ((buffer (find-file-noselect org-file)))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (goto-char (point-min))
            (when narrow-heading
              (re-search-forward (format "^\\*+ %s" (regexp-quote narrow-heading)))
              (org-back-to-heading t)
              (org-narrow-to-subtree))
            (when point-adjust-fn
              (funcall point-adjust-fn))
            (let ((org-astro-debug-images nil)
                  (org-astro-debug-console nil)
                  (org-astro-debug-log nil))
              (org-astro-export-to-mdx)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    (let* ((mdx-path (expand-file-name (concat expected-slug ".mdx") posts-dir))
           (output (with-temp-buffer
                     (insert-file-contents mdx-path)
                     (buffer-string))))
      (delete-directory temp-project t)
      output)))

(ert-deftest org-astro-replaces-date-only-title-with-heading ()
  "When #+TITLE is just a date, use the first heading for title/slug/outfile."
  (let* ((content "#+TITLE: 2025-11-23\n#+DESTINATION_FOLDER: test\n\n* First Real Title\nBody text.\n")
         (mdx (ox-astro-test--with-temp-export content "date-title.org" "first-real-title")))
    (should (string-match-p "^---" mdx))
    (should (string-match-p "title: \"First Real Title\"" mdx))))

(ert-deftest org-astro-uses-heading-when-filename-is-date ()
  "When filename is a numeric date and no title keyword, use first heading."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n* Solstice Notes\nContent here.\n")
         (mdx (ox-astro-test--with-temp-export content "2025-12-31.org" "solstice-notes")))
    (should (string-match-p "title: \"Solstice Notes\"" mdx))))

(ert-deftest org-astro-export-respects-subtree-narrowing ()
  "Narrowed subtree export should only include that subtree and derive slug from it."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n* Keep This\nBody A.\n** Nested\nDetails.\n* Ignore This\nBody B.\n")
         (mdx (ox-astro-test--with-temp-export content "subtree.org" "keep-this" "Keep This")))
    (should (string-match-p "title: \"Keep This\"" mdx))
    (should (string-match-p "Body A" mdx))
    (should (not (string-match-p "Ignore This" mdx)))
    (should (not (string-match-p "Body B" mdx)))))

(ert-deftest org-astro-narrowed-export-uses-region-root ()
  "Export the entire narrowed subtree even when point is deep inside it."
  (let* ((content "#+DESTINATION_FOLDER: test\n\n* Keep This\nBody A.\n** Nested\nDetails.\n** Deep\nMore details.\n")
         (mdx (ox-astro-test--with-temp-export
               content "subtree-point.org" "keep-this" "Keep This"
               (lambda () (goto-char (point-max))))))
    (should (string-match-p "title: \"Keep This\"" mdx))
    (should (string-match-p "Body A" mdx))
    (should (string-match-p "Details." mdx))
    (should (not (string-match-p "title: Deep" mdx)))))

(ert-deftest org-astro-reexport-does-not-duplicate-subhed ()
  "Repeated exports must not append extra #+SUBHED when one already exists."
  (let* ((temp-project (make-temp-file "ox-astro-subhed-idempotent" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (org-file (expand-file-name "subhed-idempotent.org" temp-project))
         (org-astro-known-posts-folders `(("test" . (:path ,posts-dir))))
         (org-astro-source-root-folder temp-project)
         (org-export-show-temporary-export-buffer nil)
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (content "#+TITLE: Subhed Test\n#+SUBHED: Existing excerpt\n#+DESTINATION_FOLDER: test\n\n* Heading\nParagraph.\n"))
    (make-directory posts-dir t)
    (with-temp-file org-file
      (insert content))
    (let ((buffer (find-file-noselect org-file)))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (goto-char (point-min))
            (let ((org-astro-debug-images nil)
                  (org-astro-debug-console nil)
                  (org-astro-debug-log nil))
              (org-astro-export-to-mdx)
              (org-astro-export-to-mdx)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    (with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^#\\+SUBHED:" nil t)
          (setq count (1+ count)))
        (should (= count 1))))
    (delete-directory temp-project t)))

(ert-deftest org-astro-strips-roam-links-source-preamble-from-body ()
  "Org-roam Links/Source preamble should not appear in exported MDX body."
  (let* ((content ":PROPERTIES:\n:ID: test-links-source\n:END:\n#+TITLE: Repro Links Source\n#+DESTINATION_FOLDER: test\n- Links :: [[file+emacs:/tmp/a.org][📄 A]], [[file+emacs:/tmp/b.org][📄 B]]\n- Source ::\n\n* Repro Links Source\nBody.\n")
         (mdx (ox-astro-test--with-temp-export content "links-source.org" "repro-links-source")))
    (should (string-match-p "^---" mdx))
    (should (string-match-p "# Repro Links Source" mdx))
    (should (not (string-match-p "\\[📄 A\\]" mdx)))
    (should (not (string-match-p "file:///tmp/a\\.md" mdx)))))

(ert-deftest org-astro-preserves-site-root-file-links ()
  "Root-relative site links should not become file:// URLs."
  (let* ((content "#+TITLE: Root Link Test\n#+DESTINATION_FOLDER: test\n\n* Root Link Test\n[[/media/posts/how-to-build-tidy-habits/jay-30-day-tidy-sprint.md][📄 Jay 30 Day Tidy Sprint]]\n")
         (mdx (ox-astro-test--with-temp-export content "root-link.org" "root-link-test")))
    (should (string-match-p
             "\\[📄 Jay 30 Day Tidy Sprint\\](/media/posts/how-to-build-tidy-habits/jay-30-day-tidy-sprint\\.md)"
             mdx))
    (should (not (string-match-p "file:///media/posts/how-to-build-tidy-habits" mdx)))))

(provide 'date-title-and-subtree-test)
