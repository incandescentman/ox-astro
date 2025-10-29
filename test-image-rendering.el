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
                ;; Alt text preservation.
                (should (string-match "<Image src={[^}]+} alt=\"Local Photo Described\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Space Name\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Underscore image\" />" output))
                (should (string-match "<Image src={[^}]+} alt=\"Remote fixture\" />" output))
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
          (delete-directory posts-root t))))))

(provide 'test-image-rendering)

;;; test-image-rendering.el ends here
