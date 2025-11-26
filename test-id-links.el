;;; test-id-links.el --- Tests for org-roam ID link conversion -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(defconst test-id-links--repo-root
  (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path test-id-links--repo-root)

(require 'org)
(require 'ox-astro)

(defun test-id-links--paths ()
  "Return a plist with :source-root and :posts-root for test fixtures."
  (let* ((source-root (expand-file-name "test-files/id-links" test-id-links--repo-root))
         (posts-root (expand-file-name "export" source-root)))
    (make-directory posts-root t)
    (list :source-root source-root :posts-root posts-root)))

(ert-deftest org-astro-id-link-converts-to-relative-markdown ()
  (pcase-let* ((`(:source-root ,source-root :posts-root ,posts-root) (test-id-links--paths))
               (story-file (expand-file-name "stories/impermanence-story.org" source-root))
               (mom-outfile (expand-file-name "characters/mom.mdx" posts-root))
               (story-outfile (expand-file-name "stories/the-time-mom-told-me-about-impermanence.mdx" posts-root))
               (db-file (expand-file-name ".org-roam.db" source-root)))
    (unwind-protect
        (progn
          (write-region "" nil db-file nil 'silent)
          (let ((org-astro-source-root-folder (expand-file-name "../not-the-root" source-root))
                (org-astro-known-posts-folders `(("roam" . (:path ,posts-root :preserve-folder-structure t)))))
            (let* ((id-map (org-astro--ensure-id-map org-astro-source-root-folder story-file))
                   (org-astro--id-path-map id-map)
                   (org-astro--current-outfile story-outfile)
                   (org-astro--current-output-root posts-root)
                   (org-astro--broken-link-accumulator (make-hash-table :test #'equal))
                   (org-astro--broken-link-warnings-issued (make-hash-table :test #'equal)))
              (with-temp-buffer
                (insert "[[id:char-001-mom][Mom]]")
                (org-mode)
                (let* ((parse (org-element-parse-buffer))
                       (link (org-element-map parse 'link #'identity nil t))
                       (info (list :input-file story-file)))
                  (should (equal "[Mom](../characters/mom.mdx)"
                                 (org-astro-link link "Mom" info)))))
              (should (equal mom-outfile
                             (plist-get (gethash "char-001-mom" id-map) :outfile))))))
      (when (file-exists-p db-file)
        (delete-file db-file)))))

(ert-deftest org-astro-id-link-missing-target-records-warning ()
  (pcase-let* ((`(:source-root ,source-root :posts-root ,posts-root) (test-id-links--paths))
               (story-file (expand-file-name "stories/impermanence-story.org" source-root))
               (story-outfile (expand-file-name "stories/the-time-mom-told-me-about-impermanence.mdx" posts-root)))
    (let ((org-astro-source-root-folder source-root)
          (org-astro-known-posts-folders `(("roam" . (:path ,posts-root :preserve-folder-structure t)))))
      (let* ((id-map (org-astro--ensure-id-map org-astro-source-root-folder story-file)))
        (let ((org-astro--id-path-map id-map)
              (org-astro--current-outfile story-outfile)
              (org-astro--current-output-root posts-root)
              (org-astro--broken-link-accumulator (make-hash-table :test #'equal))
              (org-astro--broken-link-warnings-issued (make-hash-table :test #'equal)))
          (with-temp-buffer
            (insert "[[id:missing-id-123][Ghost]]")
            (org-mode)
            (let* ((parse (org-element-parse-buffer))
                   (link (org-element-map parse 'link #'identity nil t))
                   (info (list :input-file story-file)))
              (should (equal "Ghost" (org-astro-link link "Ghost" info)))
              (let ((records (gethash story-outfile org-astro--broken-link-accumulator)))
                (should records)
                (should (equal "missing-id-123" (plist-get (car records) :id)))
                (should (equal "Ghost" (plist-get (car records) :text)))))))))))

(ert-deftest org-astro-broken-link-report-writes-json ()
  (pcase-let* ((`(:source-root ,source-root :posts-root ,posts-root) (test-id-links--paths))
               (story-outfile (expand-file-name "stories/the-time-mom-told-me-about-impermanence.mdx" posts-root))
               (report-path (expand-file-name "broken-links.json" posts-root)))
    (let ((hash (make-hash-table :test #'equal)))
      (puthash story-outfile (list (list :id "missing-id-123" :text "Ghost")) hash)
      (org-astro--write-broken-link-report hash posts-root)
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents report-path)
            (should (string-match "\"missing-id-123\"" (buffer-string))))
        (when (file-exists-p report-path)
          (delete-file report-path))))
    ;; Empty reports should remove the file
    (let ((hash (make-hash-table :test #'equal))
          (report-path (expand-file-name "broken-links.json" posts-root)))
      (org-astro--write-broken-link-report hash posts-root)
      (should (not (file-exists-p report-path))))))

(ert-deftest org-astro-id-link-absolute-route-mode ()
  "Test that org-astro-id-link-base-path produces absolute routes."
  (pcase-let* ((`(:source-root ,source-root :posts-root ,posts-root) (test-id-links--paths))
               (story-file (expand-file-name "stories/impermanence-story.org" source-root))
               (story-outfile (expand-file-name "stories/the-time-mom-told-me-about-impermanence.mdx" posts-root))
               (db-file (expand-file-name ".org-roam.db" source-root)))
    (unwind-protect
        (progn
          (write-region "" nil db-file nil 'silent)
          (let ((org-astro-source-root-folder (expand-file-name "../not-the-root" source-root))
                (org-astro-known-posts-folders `(("roam" . (:path ,posts-root :preserve-folder-structure t))))
                ;; Enable absolute route mode
                (org-astro-id-link-base-path "/notes"))
            (let* ((id-map (org-astro--ensure-id-map org-astro-source-root-folder story-file))
                   (org-astro--id-path-map id-map)
                   (org-astro--current-outfile story-outfile)
                   (org-astro--current-output-root posts-root)
                   (org-astro--broken-link-accumulator (make-hash-table :test #'equal))
                   (org-astro--broken-link-warnings-issued (make-hash-table :test #'equal)))
              (with-temp-buffer
                (insert "[[id:char-001-mom][Mom]]")
                (org-mode)
                (let* ((parse (org-element-parse-buffer))
                       (link (org-element-map parse 'link #'identity nil t))
                       (info (list :input-file story-file)))
                  ;; Should produce absolute route instead of relative MDX path
                  (should (equal "[Mom](/notes/characters/mom)"
                                 (org-astro-link link "Mom" info))))))))
      (when (file-exists-p db-file)
        (delete-file db-file)))))

(ert-deftest org-astro-absolute-destination-produces-outfile ()
  (let* ((source-root (make-temp-file "org-astro-src" t))
         (dest-root (make-temp-file "org-astro-dest" t))
         (subdir (expand-file-name "themes" source-root))
         (org-file (expand-file-name "themes/test-theme.org" source-root)))
    (make-directory subdir t)
    (with-temp-file org-file
      (insert ":PROPERTIES:\n:ID: theme-test\n:END:\n")
      (insert "#+TITLE: Test Theme\n")
      (insert "#+SLUG: test-theme\n")
      (insert (format "#+DESTINATION_FOLDER: %s\n\n" dest-root))
      (insert "Body\n"))
    (let ((org-astro-source-root-folder source-root))
      (let ((meta (org-astro--collect-org-file-export-metadata org-file)))
        (should (equal (plist-get meta :posts-folder) (expand-file-name dest-root)))
        (should (equal (plist-get meta :filename) "test-theme.mdx"))
        (should (string-suffix-p "/test-theme.mdx"
                                 (plist-get meta :outfile)))))))

(ert-deftest org-astro-export-sanitizes-id-stripping-hooks ()
  (let ((original-parsing '(org-export-id-link-removal helper-hook))
        (original-processing '(helper-hook org-export-id-link-removal)))
    (let ((org-export-before-parsing-functions original-parsing)
          (org-export-before-processing-functions original-processing))
      (org-astro--with-export-sanitization
        (should-not (memq 'org-export-id-link-removal org-export-before-parsing-functions))
        (should-not (memq 'org-export-id-link-removal org-export-before-processing-functions))
        (should (equal '(helper-hook) org-export-before-parsing-functions))
        (should (equal '(helper-hook) org-export-before-processing-functions)))
      ;; Outside the macro, original bindings remain unchanged.
      (should (equal original-parsing org-export-before-parsing-functions))
      (should (equal original-processing org-export-before-processing-functions)))))

(provide 'test-id-links)

;;; test-id-links.el ends here
