(require 'cl-lib)

;;; test-image-rendering.el --- Tests for image rendering pipeline -*- lexical-binding: t -*-

(defconst test-image-rendering--repo-root
  (file-name-directory (or load-file-name buffer-file-name)))

(let* ((repo-truename (directory-file-name (file-truename test-image-rendering--repo-root)))
       (load-path (cl-remove-if (lambda (dir)
                                  (string= (directory-file-name (file-truename dir)) repo-truename))
                                load-path)))
  (require 'debug)
  (require 'ert)
  (require 'org))

(add-to-list 'load-path test-image-rendering--repo-root t)

(require 'ox-astro-config)
(require 'ox-astro-helpers)
(require 'ox-astro-handlers)
(require 'ox-astro-image-handlers)
(require 'ox-astro-pdf-handlers)
(require 'ox-astro)

(defun test-image-rendering--fixture-path (relative)
  "Return the absolute path to RELATIVE within the repository."
  (expand-file-name relative test-image-rendering--repo-root))

(defun test-image-rendering--setup ()
  "Create a temporary source/destination pair populated with fixtures."
  (let* ((source-root (make-temp-file "ox-astro-image-src" t))
         (posts-root (make-temp-file "ox-astro-image-dest" t))
         (template (test-image-rendering--fixture-path "test-files/image-rendering/test-image-rendering.org"))
         (images-src (test-image-rendering--fixture-path "test-files/image-rendering/images"))
         (images-dest (expand-file-name "images" source-root)))
    (make-directory images-dest t)
    (dolist (file (directory-files images-src t "\\`[^.]"))
      (copy-file file (expand-file-name (file-name-nondirectory file) images-dest) t))
    (let* ((org-file (expand-file-name "test-image-rendering.org" source-root))
           (abs-local (replace-regexp-in-string "\\\\" "/"
                                                (convert-standard-filename
                                                 (expand-file-name "images/local-photo.png" source-root))))
           (abs-space (replace-regexp-in-string "\\\\" "/"
                                                (convert-standard-filename
                                                 (expand-file-name "images/space name image.jpg" source-root)))))
      (with-temp-buffer
        (insert-file-contents template)
        (goto-char (point-min))
        (while (search-forward "{{ABS_LOCAL}}" nil t)
          (replace-match abs-local t t))
        (goto-char (point-min))
        (while (search-forward "{{ABS_SPACE}}" nil t)
          (replace-match abs-space t t))
        (write-region (point-min) (point-max) org-file))
      (list :source-root source-root
            :posts-root posts-root
            :org-file org-file
            :remote-source (expand-file-name "remote-source.png" images-src)))))

(ert-deftest org-astro-image-rendering-produces-render-map ()
  (pcase-let* ((`(:source-root ,source-root
                 :posts-root ,posts-root
                 :org-file ,org-file
                 :remote-source ,remote-source)
                (test-image-rendering--setup)))
    (let ((org-astro-source-root-folder source-root)
          (org-astro-known-posts-folders `(("test-export" . (:path ,posts-root)))))
      (cl-letf* (((symbol-function 'org-astro--download-remote-image)
                  (lambda (_url posts-folder sub-dir)
                    (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
                           (target (expand-file-name "remote-fixture.png" assets-folder)))
                      (make-directory assets-folder t)
                      (copy-file remote-source target t)
                      target))))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect org-file)
                (unwind-protect
                    (org-astro-export-to-mdx)
                  (kill-buffer)))
              (let* ((mdx-files (directory-files-recursively posts-root "\\.mdx$"))
                     (output (and mdx-files
                                  (with-temp-buffer
                                    (insert-file-contents (car mdx-files))
                                    (buffer-string)))))
                (should (= 1 (length mdx-files)))
                (should output)
                ;; At minimum we expect four unique image imports plus the Image component import.
                (with-temp-buffer
                  (insert output)
                  (goto-char (point-min))
                  (should (>= (how-many "^import " (point-min) (point-max)) 5))
                  (goto-char (point-min))
                  (should (search-forward "import { Image } from 'astro:assets';" nil t))
                  (goto-char (point-min))
                  (should (= 7 (how-many "<Image src={" (point-min) (point-max)))))
                ;; Alt text preservation (with default responsive layout).
                (should (string-match "<Image src={[^}]+} alt=\"Local Photo Described\" layout=\"responsive\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Space Name\" layout=\"responsive\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Underscore image\" layout=\"responsive\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Remote fixture\" layout=\"responsive\" />" output))
                ;; Ensure each image import is unique.
                (let* ((import-lines (split-string output "\n"))
                       (image-import-lines
                        (cl-remove-if-not
                         (lambda (line)
                           (and (string-prefix-p "import " line)
                                (string-match "'~/assets/images/" line)))
                         import-lines)))
                  (should (= (length image-import-lines)
                             (length (cl-remove-duplicates image-import-lines :test #'string=)))))))
          (delete-directory source-root t)
          (delete-directory posts-root t)))))

(ert-deftest org-astro-export-rewrites-image-sources-in-org ()
  "Export should rewrite copied local and remote image sources in Org."
  (pcase-let* ((`(:source-root ,source-root
                 :posts-root ,posts-root
                 :org-file ,org-file
                 :remote-source ,remote-source)
                (test-image-rendering--setup))
               (expected-local (replace-regexp-in-string "\\\\" "/"
                                                         (convert-standard-filename
                                                          (expand-file-name
                                                           "src/assets/images/posts/image-rendering-test/local-photo.png"
                                                           (file-name-directory
                                                            (directory-file-name
                                                             (file-name-directory
                                                              (directory-file-name posts-root))))))))
               (expected-space (replace-regexp-in-string "\\\\" "/"
                                                         (convert-standard-filename
                                                          (expand-file-name
                                                           "src/assets/images/posts/image-rendering-test/space-name-image.jpg"
                                                           (file-name-directory
                                                            (directory-file-name
                                                             (file-name-directory
                                                              (directory-file-name posts-root))))))))
               (expected-remote (replace-regexp-in-string "\\\\" "/"
                                                          (convert-standard-filename
                                                           (expand-file-name
                                                            "src/assets/images/posts/image-rendering-test/remote-fixture.png"
                                                            (file-name-directory
                                                             (directory-file-name
                                                              (file-name-directory
                                                               (directory-file-name posts-root)))))))))
    (let ((org-astro-source-root-folder source-root)
          (org-astro-known-posts-folders `(("test-export" . (:path ,posts-root)))))
      (cl-letf* (((symbol-function 'org-astro--download-remote-image)
                  (lambda (_url posts-folder sub-dir)
                    (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
                           (target (expand-file-name "remote-fixture.png" assets-folder)))
                      (make-directory assets-folder t)
                      (copy-file remote-source target t)
                      target))))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect org-file)
                (unwind-protect
                    (org-astro-export-to-mdx)
                  (kill-buffer)))
              (let ((source-output
                     (with-temp-buffer
                       (insert-file-contents org-file)
                       (buffer-string))))
                (should (string-match-p (regexp-quote expected-local) source-output))
                (should (string-match-p (regexp-quote expected-space) source-output))
                (should (string-match-p (regexp-quote expected-remote) source-output))
                (should-not (string-match-p "https://example.com/assets/remote-fixture.png" source-output)))))
          (delete-directory source-root t)
          (delete-directory posts-root t))))))

(ert-deftest org-astro-image-layout-disabled-produces-no-layout-prop ()
  "Test that setting org-astro-image-default-layout to nil produces no layout prop."
  (pcase-let* ((`(:source-root ,source-root
                 :posts-root ,posts-root
                 :org-file ,org-file
                 :remote-source ,remote-source)
                (test-image-rendering--setup)))
    (let ((org-astro-source-root-folder source-root)
          (org-astro-known-posts-folders `(("test-export" . (:path ,posts-root))))
          ;; Disable responsive layout
          (org-astro-image-default-layout nil))
      (cl-letf* (((symbol-function 'org-astro--download-remote-image)
                  (lambda (_url posts-folder sub-dir)
                    (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
                           (target (expand-file-name "remote-fixture.png" assets-folder)))
                      (make-directory assets-folder t)
                      (copy-file remote-source target t)
                      target))))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect org-file)
                (unwind-protect
                    (org-astro-export-to-mdx)
                  (kill-buffer)))
              (let* ((mdx-files (directory-files-recursively posts-root "\\.mdx$"))
                     (output (and mdx-files
                                  (with-temp-buffer
                                    (insert-file-contents (car mdx-files))
                                    (buffer-string)))))
                (should output)
                ;; Should NOT have layout prop when disabled
                (should-not (string-match "layout=" output))
                ;; Should still have basic Image tags
                (should (string-match "<Image src={[^}]+} alt=\"Local Photo Described\" />" output))))
          (delete-directory source-root t)
          (delete-directory posts-root t))))))

(ert-deftest org-astro-image-layout-none-produces-no-layout-prop ()
  "Test that setting org-astro-image-default-layout to \"none\" produces no layout prop."
  (pcase-let* ((`(:source-root ,source-root
                 :posts-root ,posts-root
                 :org-file ,org-file
                 :remote-source ,remote-source)
                (test-image-rendering--setup)))
    (let ((org-astro-source-root-folder source-root)
          (org-astro-known-posts-folders `(("test-export" . (:path ,posts-root))))
          ;; Set to "none" string
          (org-astro-image-default-layout "none"))
      (cl-letf* (((symbol-function 'org-astro--download-remote-image)
                  (lambda (_url posts-folder sub-dir)
                    (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
                           (target (expand-file-name "remote-fixture.png" assets-folder)))
                      (make-directory assets-folder t)
                      (copy-file remote-source target t)
                      target))))
        (unwind-protect
            (progn
              (with-current-buffer (find-file-noselect org-file)
                (unwind-protect
                    (org-astro-export-to-mdx)
                  (kill-buffer)))
              (let* ((mdx-files (directory-files-recursively posts-root "\\.mdx$"))
                     (output (and mdx-files
                                  (with-temp-buffer
                                    (insert-file-contents (car mdx-files))
                                    (buffer-string)))))
                (should output)
                ;; Should NOT have layout prop when set to "none"
                (should-not (string-match "layout=" output))
                ;; Should still have basic Image tags
                (should (string-match "<Image src={[^}]+} alt=\"Local Photo Described\" />" output))))
          (delete-directory source-root t)
          (delete-directory posts-root t))))))

(ert-deftest org-astro-missing-local-image-link-is-preserved ()
  "Missing local image links should survive export as content, not disappear."
  (let* ((temp-project (make-temp-file "ox-astro-missing-image-project" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (assets-dir (expand-file-name "src/assets/images/posts" temp-project))
         (source-dir (expand-file-name "source" temp-project))
         (missing-path (expand-file-name "missing-inline.png" source-dir))
         (temp-org (make-temp-file "ox-astro-missing-image" nil ".org"))
         (output nil))
    (make-directory posts-dir t)
    (make-directory assets-dir t)
    (make-directory source-dir t)
    (with-temp-file temp-org
      (insert (format "#+TITLE: Missing Image Preservation Test\n#+SLUG: missing-image-preservation-test\n#+DESTINATION_FOLDER: jaydocs\n\n* Body\n[[%s]]\n"
                      missing-path)))
    (unwind-protect
        (let ((org-export-show-temporary-export-buffer nil)
              (org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-persist-directory (expand-file-name "org-persist" temp-project))
              (org-astro-debug-images nil)
              (org-astro-debug-console nil)
              (org-astro-debug-log nil)
              (org-astro-known-posts-folders `(("jaydocs" . (:path ,posts-dir))))
              (org-astro-source-root-folder (file-name-directory temp-org)))
          (make-directory org-persist-directory t)
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
          (let ((mdx-path (expand-file-name "missing-image-preservation-test.mdx" posts-dir)))
            (setq output
                  (with-temp-buffer
                    (insert-file-contents mdx-path)
                    (buffer-string)))))
      (when (file-exists-p temp-org)
        (delete-file temp-org))
      (when (file-exists-p temp-project)
        (delete-directory temp-project t)))
    (should output)
    (should (string-match-p "missing-inline\\.png" output))
    (should-not (string-match-p "<Image src={" output))))

(ert-deftest org-astro-missing-current-app-asset-path-still-exports-image ()
  "Missing paths already inside the current app assets tree should export as Astro images."
  (let* ((temp-project (make-temp-file "ox-astro-missing-app-asset-project" t))
         (posts-dir (expand-file-name "src/content/blog" temp-project))
         (assets-dir (expand-file-name "src/assets/images/posts/lowercase-slug" temp-project))
         (missing-path (expand-file-name "missing-inline.png" assets-dir))
         (temp-org (make-temp-file "ox-astro-missing-app-asset" nil ".org"))
         (output nil))
    (make-directory posts-dir t)
    (make-directory assets-dir t)
    (with-temp-file temp-org
      (insert (format "#+TITLE: Missing Current App Asset Test\n#+SLUG: Mixed-Case-Slug\n#+DESTINATION_FOLDER: jaydocs\n\n* Body\n[[%s]]\n"
                      missing-path)))
    (unwind-protect
        (let ((org-export-show-temporary-export-buffer nil)
              (org-export-with-toc nil)
              (org-export-with-section-numbers nil)
              (org-persist-directory (expand-file-name "org-persist" temp-project))
              (org-astro-debug-images nil)
              (org-astro-debug-console nil)
              (org-astro-debug-log nil)
              (org-astro-known-posts-folders `(("jaydocs" . (:path ,posts-dir))))
              (org-astro-source-root-folder (file-name-directory temp-org)))
          (make-directory org-persist-directory t)
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
          (let ((mdx-path (expand-file-name "Mixed-Case-Slug.mdx" posts-dir)))
            (setq output
                  (with-temp-buffer
                    (insert-file-contents mdx-path)
                    (buffer-string)))))
      (when (file-exists-p temp-org)
        (delete-file temp-org))
      (when (file-exists-p temp-project)
        (delete-directory temp-project t)))
    (should output)
    (should (string-match-p "import { Image } from 'astro:assets';" output))
    (should (string-match-p "~/assets/images/posts/lowercase-slug/missing-inline\\.png" output))
    (should (string-match-p "<Image src={[^}]+} alt=\"Missing inline\"" output))))

(ert-deftest org-astro-format-image-component-includes-layout ()
  "Unit test for org-astro--format-image-component with layout prop."
  ;; Test with responsive layout (default)
  (let ((org-astro-image-default-layout "responsive"))
    (should (string= (org-astro--format-image-component "myImage" "Alt text")
                     "<Image src={myImage} alt=\"Alt text\" layout=\"responsive\" />")))
  ;; Test with fixed layout
  (let ((org-astro-image-default-layout "fixed"))
    (should (string= (org-astro--format-image-component "myImage" "Alt text")
                     "<Image src={myImage} alt=\"Alt text\" layout=\"fixed\" />")))
  ;; Test with nil (no layout)
  (let ((org-astro-image-default-layout nil))
    (should (string= (org-astro--format-image-component "myImage" "Alt text")
                     "<Image src={myImage} alt=\"Alt text\" />")))
  ;; Test with "none" (no layout)
  (let ((org-astro-image-default-layout "none"))
    (should (string= (org-astro--format-image-component "myImage" "Alt text")
                     "<Image src={myImage} alt=\"Alt text\" />"))))

(provide 'test-image-rendering)

;;; test-image-rendering.el ends here
