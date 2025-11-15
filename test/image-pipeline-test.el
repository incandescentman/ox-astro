(require 'ert)
(defconst ox-astro-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))
(add-to-list 'load-path ox-astro-test--repo-root)
(require 'ox-astro)
(require 'org)
(require 'subr-x)
(require 'cl-lib)

(defconst ox-astro-test--gallery-org
  "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/test/gallery_test_rigorous.org")

(defconst ox-astro-test--gallery-expected
  "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/test/gallery_test_rigorous.expected.mdx")

(defconst ox-astro-test--gallery-slug "gallery-test-rigorous")

(defun ox-astro-test--normalize-string (s)
  "Normalize S by removing generated comment metadata and unstable tokens."
  (let* ((no-comment (replace-regexp-in-string "{/\\* Source org: [^}]* \\*/}\n" "" s))
         (lines (split-string no-comment "\n"))
         (filtered-lines (cl-remove-if
                          (lambda (line)
                            (let ((trimmed (string-trim-right line)))
                              (or (string-prefix-p "-   **Links:**" trimmed)
                                  (string-prefix-p "-   **Source:**" trimmed))))
                          lines))
         (no-metadata (string-join filtered-lines "\n"))
         (normalized-ids (replace-regexp-in-string "galleryId=\"gallery-[0-9]+\""
                                                   "galleryId=\"gallery-XXX\""
                                                   no-metadata))
         (collapsed (replace-regexp-in-string "\n\\{3,\\}" "\n\n" normalized-ids)))
    (string-trim collapsed)))

(defun ox-astro-test--count-occurrences (needle haystack)
  "Count non-overlapping occurrences of NEEDLE inside HAYSTACK."
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun ox-astro-test--export-gallery ()
  "Export the gallery test org file and return the produced MDX string.
The export runs against a temporary copy of the source file and writes
to a temporary destination folder so the real workspace stays untouched."
  (let* ((temp-project (make-temp-file "ox-astro-gallery-project" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (assets-dir (expand-file-name "src/assets/images/posts" temp-project))
         (temp-org (make-temp-file "ox-astro-gallery" nil ".org"))
         (org-export-show-temporary-export-buffer nil)
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (org-astro-debug-images nil)
         (org-astro-debug-console nil)
         (org-astro-debug-log nil)
         (org-astro-known-posts-folders
          `(("jaydocs" . (:path ,posts-dir))))
         (org-astro-source-root-folder (file-name-directory temp-org)))
    (make-directory posts-dir t)
    (make-directory assets-dir t)
    (copy-file ox-astro-test--gallery-org temp-org t)
    (let ((buffer (find-file-noselect temp-org)))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (let ((inhibit-message t))
              (org-astro-export-to-mdx)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    (let* ((mdx-path (expand-file-name
                      (concat ox-astro-test--gallery-slug ".mdx")
                      posts-dir))
           (output (with-temp-buffer
                     (insert-file-contents mdx-path)
                     (buffer-string))))
      (delete-directory temp-project t)
      (delete-file temp-org)
      output)))

(ert-deftest ox-astro-gallery-export-test ()
  "Test the export of the rigorous gallery Org file."
  (let* ((actual-output (ox-astro-test--export-gallery))
         (expected-output
          (with-temp-buffer
            (insert-file-contents ox-astro-test--gallery-expected)
            (buffer-string))))

    ;; 1. Compare the overall output, ignoring source comment and whitespace
    (should (string= (ox-astro-test--normalize-string actual-output)
                     (ox-astro-test--normalize-string expected-output)))

    ;; 2. Assert the number of ImageGallery components
    (should (= (ox-astro-test--count-occurrences "<ImageGallery" actual-output) 6))

    ;; 3. Assert the number of standalone Image components
    (should (= (ox-astro-test--count-occurrences "<Image src=" actual-output) 2))))

(ert-deftest org-astro-final-output-filter-preserves-code-fences ()
  "Indented lines inside fenced code blocks should not become blockquotes."
  (let* ((input (mapconcat #'identity
                           '("```js"
                             "    const foo = 42;"
                             "```"
                             ""
                             "    Quoted paragraph")
                           "\n"))
         (result (org-astro-final-output-filter input nil (list))))
    (should (string-match-p "```js\n    const foo = 42;\n```" result))
    (should (string-match-p "\n> Quoted paragraph" result))))

(ert-deftest org-astro-final-output-filter-preserves-list-indents ()
  "Nested list continuation lines must remain indented text."
  (let* ((input "- Item\n    continuation line\n")
         (result (org-astro-final-output-filter input nil (list))))
    (should (string-match-p "- Item" result))
    (should (string-match-p "\n    continuation line" result))
    (should-not (string-match-p "\n> continuation line" result))))

(provide 'image-pipeline-test)
