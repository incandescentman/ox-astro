;;; ox-astro-helpers.el --- Helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:

(require 'subr-x) ; for string-trim, string-trim-right
(require 'cl-lib)
(require 'json)

;; Declare functions from other ox-astro modules
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--process-pdf-path "ox-astro-pdf-handlers")
(declare-function org-astro--collect-pdfs-from-tree "ox-astro-pdf-handlers")
(declare-function org-astro--collect-images-from-tree "ox-astro-image-handlers")
(declare-function org-astro--collect-raw-images-from-tree-region "ox-astro-image-handlers")
(declare-function org-astro--update-source-buffer-image-path "ox-astro-image-handlers")
(declare-function org-astro--parse-tags "ox-astro-metadata")
(declare-function org-astro--parse-categories "ox-astro-metadata")
(declare-function org-astro--parse-places "ox-astro-metadata")
(declare-function org-astro--parse-people "ox-astro-metadata")
(declare-function org-astro--parse-emotions "ox-astro-metadata")
(declare-function org-astro--parse-themes "ox-astro-metadata")
(declare-function org-astro--get-date-occurred "ox-astro-metadata")
(declare-function org-astro--get-era "ox-astro-metadata")
(declare-function org-astro--get-place "ox-astro-metadata")
(declare-function org-astro--get-story-type "ox-astro-metadata")

;; Declare global variable for data persistence across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

(defvar org-astro--id-path-map nil
  "Hash table mapping org-roam IDs to export metadata for the current run.")

(defvar org-astro--broken-link-accumulator nil
  "Hash table mapping output files to unresolved org-roam ID links.")

(defvar org-astro--broken-link-warnings-issued nil
  "Hash table used to deduplicate warnings for missing org-roam IDs.")

(defvar org-astro--current-outfile nil
  "Absolute path to the MDX file currently being generated.")

(defvar org-astro--current-output-root nil
  "Root output directory for the current export destination.")

;; Simple debug logging function that writes directly to file
(defun org-astro--debug-log-direct (fmt &rest args)
  "Write debug message directly to log file when debugging is enabled."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((msg (apply #'format fmt args))
           (timestamp (format-time-string "%H:%M:%S"))
           (line (format "[%s] %s\n" timestamp msg))
           (debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-debug.log")))
      (condition-case _
          (with-temp-buffer
            (insert line)
            (write-region (point-min) (point-max) debug-file t 'silent))
        (error nil))
      (message "[ox-astro] %s" msg))))

;; Debug helpers
(defun org-astro--dbg-log (info fmt &rest args)
  "Append a formatted debug message to INFO and write to ox-astro-debug.log when enabled."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((msg (apply #'format fmt args))
           (existing (plist-get info :astro-debug-log))
           (timestamp (format-time-string "%H:%M:%S"))
           (line (format "[%s] %s\n" timestamp msg))
           (debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-debug.log"))
           ;; Check if this is the first log entry
           (first-entry (null existing)))
      ;; Store in info for potential MDX comments or later inspection
      (plist-put info :astro-debug-log (cons msg existing))
      ;; Emit to Messages
      (message "[ox-astro] %s" msg)
      ;; Write header if first entry
      (when first-entry
        (let* ((source-file (or (plist-get info :input-file)
                                (and (buffer-file-name) (expand-file-name (buffer-file-name)))
                                "unknown"))
               ;; Try to get actual output file from info if available, otherwise estimate
               (actual-output-file (plist-get info :astro-actual-output-file))
               (posts-folder (or (plist-get info :destination-folder)
                                 (plist-get info :astro-posts-folder)))
               ;; Determine output file path (will be updated later with actual path)
               (output-file (or actual-output-file
                                (when posts-folder
                                  (let* ((title (org-astro--get-title (plist-get info :parse-tree) info))
                                         (slug (when title (org-astro--slugify title)))
                                         (filename (if slug (concat slug ".mdx") "exported-file.mdx"))
                                         (resolved-folder (cdr (assoc posts-folder org-astro-known-posts-folders))))
                                    (when resolved-folder
                                      (expand-file-name filename resolved-folder))))))
               (clipboard-text (format "Source: %s\nOutput: %s\nDebug: %s"
                                       source-file
                                       (or output-file "[determining...]")
                                       debug-file))
               (file-header (format "========================================\nOX-ASTRO DEBUG LOG - %s\n========================================\nSource: %s\nOutput: %s\n========================================\n\n"
                                    (format-time-string "%Y-%m-%d %H:%M:%S")
                                    source-file
                                    (or output-file "[determining...]"))))
          ;; Store header info for potential updates
          (plist-put info :astro-debug-header-info (list :source source-file :output output-file :debug debug-file))
          ;; Write header to debug file
          (condition-case _
              (with-temp-buffer
                (insert file-header)
                (write-region (point-min) (point-max) debug-file nil 'silent))
            (error nil))
          ;; Copy clipboard text to clipboard via pbcopy
          (let ((pbcopy (executable-find "pbcopy")))
            (when pbcopy
              (condition-case _
                  (with-temp-buffer
                    (insert clipboard-text)
                    (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                (error nil))))))
      ;; Append the actual log line
      (condition-case _
          (with-temp-buffer
            (insert line)
            (write-region (point-min) (point-max) debug-file t 'silent))
        (error nil)))))

(defun org-astro--dbg-update-output-file (info actual-output-file)
  "Update the debug file header with the actual output file path."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images actual-output-file)
    (let* ((debug-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-debug.log"))
           (header-info (plist-get info :astro-debug-header-info)))
      (when header-info
        (let* ((source-file (plist-get header-info :source))
               (clipboard-text (format "Source: %s\nOutput: %s\nDebug: %s"
                                       source-file actual-output-file debug-file))
               (file-header (format "========================================\nOX-ASTRO DEBUG LOG - %s\n========================================\nSource: %s\nOutput: %s\n========================================\n\n"
                                    (format-time-string "%Y-%m-%d %H:%M:%S")
                                    source-file actual-output-file)))
          ;; Update the debug file header
          (condition-case _
              (when (file-exists-p debug-file)
                (with-temp-buffer
                  (insert-file-contents debug-file)
                  (goto-char (point-min))
                  ;; Look for the end of the header section (first log entry or empty line after header)
                  (if (re-search-forward "^\\(\\[.*\\]\\|$\\)" nil t)
                      (progn
                        (beginning-of-line)
                        ;; Delete the old header
                        (delete-region (point-min) (point))
                        ;; Insert the new header
                        (goto-char (point-min))
                        (insert file-header))
                      ;; If no log entries found, replace everything with new header
                      (erase-buffer)
                      (insert file-header))
                  (write-region (point-min) (point-max) debug-file nil 'silent)))
            (error nil))
          ;; Update clipboard
          (let ((pbcopy (executable-find "pbcopy")))
            (when pbcopy
              (condition-case _
                  (with-temp-buffer
                    (insert clipboard-text)
                    (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                (error nil)))))))))

(defun org-astro--dbg-mdx-comments (info)
  "Return MDX comments string for any collected debug messages in INFO."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let ((log (plist-get info :astro-debug-log)))
      (when log
        (concat (mapconcat (lambda (s) (format "{/* %s */}" s)) (nreverse log) "\n")
                "\n\n")))))

;; Org-roam ID link helpers
(defun org-astro--normalize-display-path (path)
  "Return PATH relative to `org-astro-source-root-folder' when possible."
  (when path
    (let* ((abs (expand-file-name path))
           (root (and org-astro-source-root-folder
                      (expand-file-name org-astro-source-root-folder))))
      (cond
       ((and root (file-directory-p root) (string-prefix-p root abs))
        (file-relative-name abs root))
       (t
        (file-name-nondirectory abs))))))

(defun org-astro--resolve-destination-config (value)
  "Resolve VALUE (nickname or path) to an output directory description.
Returns a plist with keys :path, :preserve, :nickname, :raw."
  (let* ((trim (and value (string-trim value)))
         (config (and trim
                      (cdr (cl-find trim org-astro-known-posts-folders
                                    :test (lambda (needle pair)
                                            (string= needle (string-trim (car pair))))))))
         (path (or (and (stringp config) config)
                   (and (listp config) (plist-get config :path))
                   (and trim (file-name-absolute-p trim) trim)))
         (preserve (and (listp config)
                        (plist-get config :preserve-folder-structure))))
    (when path
      (setq path (expand-file-name path)))
    (list :path path
          :preserve preserve
          :nickname (when (and trim config) trim)
          :raw trim)))

(defun org-astro--relative-subdir-for-file (file &optional source-root)
  "Return relative directory for FILE under SOURCE-ROOT preserving structure."
  (let* ((root (and (or source-root org-astro-source-root-folder)
                    (expand-file-name (or source-root org-astro-source-root-folder))))
         (abs-file (expand-file-name file)))
    (when (and root (file-directory-p root) (string-prefix-p root abs-file))
      (let ((relative (file-relative-name abs-file root)))
        (unless (string-prefix-p ".." relative)
          (let ((dir (file-name-directory relative)))
            (when (and dir (not (member dir '("" "./"))))
              dir)))))))

(defun org-astro--keyword-value (tree key-or-keys)
  "Return trimmed value for KEY-OR-KEYS from TREE, or nil if missing.

KEY-OR-KEYS may be a single string keyword or a list of keyword strings.
This helper respects the first matching keyword encountered in TREE."
  (let ((search-keys (if (listp key-or-keys) key-or-keys (list key-or-keys))))
    (cl-some
     (lambda (key)
       (org-element-map tree 'keyword
         (lambda (kw)
           (when (string-equal key (org-element-property :key kw))
             (let ((value (org-element-property :value kw)))
               (when value (string-trim value)))))
         nil 'first-match))
     search-keys)))

(defun org-astro--determine-export-filename (file slug)
  "Derive the MDX filename for FILE given SLUG (when present)."
  (if (and slug (not (string-blank-p slug)))
      (concat slug ".mdx")
    (let* ((base (file-name-base file))
           (trimmed (replace-regexp-in-string "^[0-9]+-" "" base))
           (hyphenated (replace-regexp-in-string "_" "-" trimmed)))
      (concat hyphenated ".mdx"))))

(defun org-astro--collect-org-file-export-metadata (file)
  "Return metadata plist describing how FILE exports, including IDs."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (let ((case-fold-search t)
              (ids nil)
              slug title astro-folder dest-folder)
          (goto-char (point-min))
          (while (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
            (let ((id (string-trim (match-string 1))))
              (when (and id (not (string-empty-p id)))
                (cl-pushnew id ids :test #'equal))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+SLUG:\\s-*\\(.+\\)$" nil t)
            (setq slug (string-trim (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
            (setq title (string-trim (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+ASTRO_POSTS_FOLDER:\\s-*\\(.+\\)$" nil t)
            (setq astro-folder (string-trim (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+DESTINATION[_-]FOLDER:\\s-*\\(.+\\)$" nil t)
            (setq dest-folder (string-trim (match-string 1))))
          (unless title
            (goto-char (point-min))
            (when (re-search-forward "^\\*+ \\(.+\\)$" nil t)
              (setq title (string-trim (match-string 1)))))
          (let* ((slug-final (if (and slug (not (string-blank-p slug)))
                                 slug
                               (when (and title (not (string-blank-p title)))
                                 (org-astro--slugify title))))
                 (destination-info (org-astro--resolve-destination-config
                                    (or astro-folder dest-folder)))
                 (posts-folder (plist-get destination-info :path))
                 (preserve (plist-get destination-info :preserve))
                 (relative-subdir (and preserve
                                       (org-astro--relative-subdir-for-file file)))
                 (pub-dir (cond
                           ((and posts-folder relative-subdir)
                            (expand-file-name relative-subdir posts-folder))
                           (posts-folder posts-folder)
                           (t nil)))
                 (filename (org-astro--determine-export-filename file slug-final))
                 (outfile (when pub-dir
                            (expand-file-name filename pub-dir))))
            (list :id-list (nreverse ids)
                  :slug slug-final
                  :title title
                  :posts-folder posts-folder
                  :preserve preserve
                  :relative-subdir relative-subdir
                  :outfile outfile
                  :filename filename
                  :destination-raw (plist-get destination-info :raw)
                  :source file))))
    (error
     (message "[ox-astro] Failed to inspect %s for ID metadata: %s"
              file (error-message-string err))
     nil)))

(defun org-astro--id-map-store (map id meta)
  "Store ID mapping derived from META into MAP."
  (let ((entry (list :source (plist-get meta :source)
                     :outfile (plist-get meta :outfile)
                     :filename (plist-get meta :filename)
                     :posts-folder (plist-get meta :posts-folder)
                     :relative-subdir (plist-get meta :relative-subdir)
                     :preserve (plist-get meta :preserve)
                     :slug (plist-get meta :slug)
                     :title (plist-get meta :title)
                     :destination-raw (plist-get meta :destination-raw))))
    (when-let ((existing (gethash id map)))
      (unless (string-equal (plist-get existing :source)
                            (plist-get meta :source))
        (message "[ox-astro] Duplicate org-roam ID %s found in %s (already mapped to %s)"
                 id (plist-get meta :source) (plist-get existing :source))))
    (puthash id entry map)))

(defun org-astro--build-id-map (source-dir)
  "Build and return a hash table mapping org-roam IDs in SOURCE-DIR."
  (let* ((root (and source-dir (expand-file-name source-dir)))
         (map (make-hash-table :test #'equal)))
    (if (and root (file-directory-p root))
        (dolist (file (directory-files-recursively root "\\.org\\'"))
          (let ((meta (org-astro--collect-org-file-export-metadata file)))
            (when meta
              (dolist (id (plist-get meta :id-list))
                (when (and id (not (string-blank-p id)))
                  (org-astro--id-map-store map id meta))))))
      (when root
        (message "[ox-astro] ID map skipped: source directory %s not found" root)))
    map))

(defun org-astro--add-file-to-id-map (map file)
  "Ensure FILE's IDs are present in MAP."
  (when (and map file (file-exists-p file))
    (let ((meta (org-astro--collect-org-file-export-metadata file)))
      (when meta
        (dolist (id (plist-get meta :id-list))
          (when (and id (not (string-blank-p id)))
            (org-astro--id-map-store map id meta))))))
  map)

(defun org-astro--effective-source-root (source-dir current-file)
  "Return the best source root to scan for org-roam IDs.
SOURCE-DIR is the configured `org-astro-source-root-folder'.  If CURRENT-FILE
is not under SOURCE-DIR, fall back to a detected root by searching for
`.org-roam.db' or `.git' directories, or ultimately the file's parent."
  (let* ((configured (and source-dir (expand-file-name source-dir)))
         (file (and current-file (expand-file-name current-file))))
    (cond
     ;; Configured root covers the current file → use it
     ((and configured file
           (file-directory-p configured)
           (string-prefix-p (file-name-as-directory configured)
                            (file-name-directory file)))
      configured)
     ;; Try to detect org-roam style root (presence of .org-roam.db)
     ((and file (locate-dominating-file file ".org-roam.db")))
     ;; Fall back to enclosing git repo root if available
     ((and file (locate-dominating-file file ".git")))
     ;; As a last resort, use the current file's directory
     (file
      (file-name-directory file))
     (t configured))))

(defun org-astro--ensure-id-map (source-dir current-file)
  "Build an ID map rooted at SOURCE-DIR and ensure CURRENT-FILE is included."
  (let* ((root (org-astro--effective-source-root source-dir current-file))
         ;; Ensure helper functions that rely on `org-astro-source-root-folder'
         ;; (like relative subdir detection) see the effective root.
         (org-astro-source-root-folder root))
    (let ((map (org-astro--build-id-map root)))
      (org-astro--add-file-to-id-map map current-file))))

(defun org-astro--calculate-relative-mdx-path (from-file to-file)
  "Return a Markdown-friendly relative path from FROM-FILE to TO-FILE."
  (when (and from-file to-file)
    (let* ((from-dir (file-name-directory from-file))
           (relative (file-relative-name to-file from-dir)))
      (cond
       ((or (null relative)
            (string= relative "")
            (string= relative "."))
        "./")
       ((or (string-prefix-p "./" relative)
            (string-prefix-p "../" relative))
        (if (string= relative "./")
            "./"
          relative))
       (t
        (concat "./" relative))))))

(defun org-astro--record-missing-id-link (info target-id desc)
  "Log and record a missing TARGET-ID using INFO and DESC.
Returns fallback text that should be inserted into the document."
  (let* ((text (or (and desc (string-trim desc)) target-id))
         (source-file (or (plist-get info :input-file)
                          (and (buffer-file-name)
                               (expand-file-name (buffer-file-name)))))
         (display (org-astro--normalize-display-path source-file))
         (warning-key (format "%s::%s::%s"
                              (or org-astro--current-outfile "[unknown]")
                              target-id text)))
    (unless (and org-astro--broken-link-warnings-issued
                 (gethash warning-key org-astro--broken-link-warnings-issued))
      (message "⚠️  Broken link in %s:\n    [[id:%s][%s]] - target not found"
               (or display "unknown")
               target-id text)
      (when org-astro--broken-link-warnings-issued
        (puthash warning-key t org-astro--broken-link-warnings-issued)))
    (when (and org-astro--broken-link-accumulator org-astro--current-outfile)
      (let* ((existing (gethash org-astro--current-outfile org-astro--broken-link-accumulator))
             (new-entry (list :id target-id :text text :source source-file)))
        (unless (cl-find-if (lambda (item)
                              (and (string= (plist-get item :id) target-id)
                                   (string= (plist-get item :text) text)))
                            existing)
          (puthash org-astro--current-outfile (cons new-entry existing)
                   org-astro--broken-link-accumulator))))
    text))

(defun org-astro--write-broken-link-report (links-hash output-root)
  "Persist LINKS-HASH as JSON inside OUTPUT-ROOT.
Existing reports are cleared when there are no broken links."
  (when output-root
    (let* ((report-path (expand-file-name "broken-links.json" output-root))
           (entries nil))
      (maphash
       (lambda (outfile items)
         (let* ((abs-outfile (expand-file-name outfile))
                (abs-root (and output-root (expand-file-name output-root)))
                (relative (if (and abs-root (string-prefix-p abs-root abs-outfile))
                              (file-relative-name abs-outfile abs-root)
                            (or (org-astro--normalize-display-path outfile)
                                (file-name-nondirectory outfile))))
                (payload (mapcar (lambda (item)
                                   (list (cons "id" (plist-get item :id))
                                         (cons "text" (plist-get item :text))))
                                 (nreverse items))))
           (push (cons relative payload) entries)))
       links-hash)
      (if entries
          (let* ((sorted (sort entries (lambda (a b) (string< (car a) (car b)))))
                 (json-object-type 'alist)
                 (json-array-type 'list)
                 (json-encoding-pretty-print t))
            (with-temp-file report-path
              (insert (json-encode sorted)))
            (message "[ox-astro] Wrote broken link report to %s" report-path))
        (when (file-exists-p report-path)
          (delete-file report-path)
          (message "[ox-astro] Cleared broken link report at %s (no broken links)" report-path))))))

;; Insert or replace a #+KEY: VALUE line in the top keyword block.
(defun org-astro--upsert-keyword (key value)
  "Upsert #+KEY: VALUE near the top of the buffer or narrowed region.
If KEY exists before the first headline, replace its line.
Otherwise insert after the existing keyword/comment block.
Respects narrowing - works within the current narrowed region."
  (let* ((ukey (upcase (format "%s" key)))
         (re (format "^#\\+%s:\\s-*\\(.*\\)$" (regexp-quote ukey))))
    (save-excursion
      (goto-char (point-min))
      (let ((limit (save-excursion
                     (or (re-search-forward "^\\*" nil t) (point-max)))))
        (if (re-search-forward re limit t)
            ;; Replace existing line
            (progn
              (beginning-of-line)
              (kill-line)
              (insert (format "#+%s: %s" ukey (or value ""))))
            ;; Insert at end of keyword/comment/properties block
            (goto-char (point-min))
            ;; Check if we're in a narrowed subtree (starts with heading)
            (if (looking-at-p "^\\*+ ")
                ;; In narrowed subtree: place keywords after the heading and any properties
                (progn
                  (forward-line 1)  ; Skip the heading line
                  ;; Skip over any properties block
                  (when (looking-at-p "^:PROPERTIES:")
                    (forward-line 1)
                    (while (and (< (point) limit)
                                (not (looking-at-p "^:END:")))
                      (forward-line 1))
                    (when (looking-at-p "^:END:")
                      (forward-line 1)))
                  ;; Skip existing keywords and blank lines
                  (while (and (< (point) limit)
                              (or (looking-at-p "^#\\+")
                                  (looking-at-p "^#\\s-")
                                  (looking-at-p "^\\s-*$")))
                    (forward-line 1)))
                ;; In full file: prefer placing after org-roam preamble (- Links :: / - Source ::)
                (progn
                  ;; First, skip over any org-roam properties block
                  (when (looking-at-p "^:PROPERTIES:")
                    (forward-line 1)
                    (while (and (< (point) limit)
                                (not (looking-at-p "^:END:")))
                      (forward-line 1))
                    (when (looking-at-p "^:END:")
                      (forward-line 1)))
                  ;; If there is a roam preamble (- Links :: or - Source ::) before first headline,
                  ;; insert after the last such line; otherwise fall back to after existing keywords/comments.
                  (let ((roam-anchor (save-excursion
                                       (let ((last-pos nil))
                                         (save-restriction
                                           (narrow-to-region (point-min) limit)
                                           (goto-char (point-min))
                                           (while (re-search-forward "^-[ \t]+\(Links\|Source\) ::[ \t]*$" nil t)
                                             (setq last-pos (line-end-position))))
                                         (when last-pos
                                           (goto-char last-pos)
                                           (forward-line 1)
                                           (point))))))
                    (if roam-anchor
                        (goto-char roam-anchor)
                        ;; Fall back: skip over keywords, comments, and blank lines
                        (while (and (< (point) limit)
                                    (or (looking-at-p "^#\\+")
                                        (looking-at-p "^#\\s-")
                                        (looking-at-p "^\\s-*$")))
                          (forward-line 1))))))
            ;; Keep one blank line before insert unless at BOF or already blank
            (unless (or (bobp) (looking-at-p "^\\s-*$"))
              (insert "\n"))
            (insert (format "#+%s: %s\n" ukey (or value ""))))))))

;; Back-compat alias for existing calls
(defun org-astro--insert-keyword-at-end-of-block (key value)
  "Alias for `org-astro--upsert-keyword'."
  (org-astro--upsert-keyword key value))

(defun org-astro--upsert-keyword-after-roam (key value)
  "Insert #+KEY: VALUE after org-roam preamble if present, else use normal placement."
  (save-excursion
    (goto-char (point-min))
    (let* ((limit (save-excursion (or (re-search-forward "^\\*" nil t) (point-max))))
           (anchor (save-excursion
                     (save-restriction
                       (narrow-to-region (point-min) limit)
                       (goto-char (point-min))
                       (let ((last-pos nil))
                         (while (re-search-forward "^-[ \t]+\\(Links\\|Source\\) ::[ \t]*$" nil t)
                           (setq last-pos (line-end-position)))
                         (when last-pos (goto-char last-pos) (forward-line 1) (point)))))))
      (if anchor
          (progn (goto-char anchor)
                 (unless (or (bobp) (looking-at-p "^\\s-*$")) (insert "\n"))
                 (insert (format "#+%s: %s\n" (upcase key) value)))
          (org-astro--upsert-keyword key value)))))

(defun org-astro--normalize-user-blocks ()
  "Convert org headings to markdown inside user/prompt/quote blocks.
This prevents org-mode from interpreting asterisks as headings inside
these special blocks, which would break the block structure."
  (save-excursion
    (goto-char (point-min))
    (let ((modified nil))
      (while (re-search-forward "^#\\+begin_src \\(user\\|prompt\\|quote\\)" nil t)
        (let ((block-start (point))
              (block-end (save-excursion
                          (when (re-search-forward "^#\\+end_src" nil t)
                            (match-beginning 0)))))
          (when block-end
            (save-restriction
              (narrow-to-region block-start block-end)
              (goto-char (point-min))
              ;; Convert org headings to markdown (must go from most to least asterisks)
              (while (re-search-forward "^\\(\\*\\*\\*\\*\\) \\(.*\\)$" nil t)
                (replace-match "#### \\2")
                (setq modified t))
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*\\*\\*\\) \\(.*\\)$" nil t)
                (replace-match "### \\2")
                (setq modified t))
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*\\*\\) \\(.*\\)$" nil t)
                (replace-match "## \\2")
                (setq modified t))
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\*\\) \\(.*\\)$" nil t)
                (replace-match "# \\2")
                (setq modified t)))
            ;; Move past this block to continue searching
            (goto-char block-end))))
      (when modified
        (message "[ox-astro] Auto-converted org headings to markdown in user/prompt/quote blocks")))))

(defun org-astro--safe-export (data info)
  "Like `org-export-data' but never throws. Falls back to readable plain text."
  (condition-case _
      (org-trim (org-export-data data info))
    (error
     (let* ((s (org-no-properties (org-element-interpret-data data))))
       ;; [[url][desc]] -> desc, [[url]] -> url
       (setq s (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" "\\2" s))
       (setq s (replace-regexp-in-string "\\[\\[\\([^]]+\\)\\]\\]" "\\1" s))
       ;; Drop common inline markup and collapse whitespace/newlines
       (setq s (replace-regexp-in-string "[*_~/=]" "" s))
       (string-trim (replace-regexp-in-string "\n+" " " s))))))

(defun org-astro--slugify (s)
  "Convert string S to a slug."
  (when (stringp s)
    (let ((s (downcase s)))
      (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil))))

;; Detect whether TEXT contains Markdown link syntax that should be preserved
;; as-is. We check for inline links [text](url) and reference-style links
;; [text][ref]. This is intentionally conservative to avoid false positives.
(defun org-astro--contains-markdown-link-p (text)
  "Return non-nil if TEXT contains Markdown link syntax.
Matches inline links like [label](https://example.com) and
reference-style links like [label][ref]."
  (when (and text (stringp text))
    (or (string-match-p "\\[[^]]+\\](\\([^)]\\|\\n\\)+)" text) ; inline [..](..)
        (string-match-p "\\[[^]]+\\]\\[[^]]+\\]" text)))   ; reference [..][..]
  )

;; Detect Markdown reference-style link definitions lines like:
;;   [1]: https://example.com "Title"
;;   [label]: http://example.com

(defun org-astro--markdown-link-definition-line-p (text)
  "Return non-nil if TEXT looks like a Markdown reference link definition."
  (when (and text (stringp text))
    (string-match-p "^[\t ]*\\[[^]]+\\]:[\t ]+https?://[^\t ]+.*$" (string-trim-right text))))

(defun org-astro--markdown-link-definition-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH in the buffer looks like a Markdown link definition."
  (when (and paragraph (eq (org-element-type paragraph) 'paragraph))
    (let* ((beg (org-element-property :begin paragraph))
           (end (org-element-property :end paragraph))
           (raw (and beg end (buffer-substring-no-properties beg end))))
      (and raw (org-astro--markdown-link-definition-line-p raw)))))

(defun org-astro--format-date (date-raw info)
  "Format DATE-RAW into a string suitable for Astro front matter."
  (let ((date-fmt (plist-get info :astro-date-format)))
    (format-time-string
     date-fmt
     (org-time-string-to-time date-raw))))

(defun org-astro--filename-to-alt-text (path)
  "Generate a human-readable alt text from an image PATH."
  (when (stringp path)
    (let* ((filename (file-name-sans-extension (file-name-nondirectory path)))
           ;; Replace hyphens and underscores with spaces
           (human-readable (replace-regexp-in-string "[-_]" " " filename))
           ;; Remove YYYY MM DD and long number runs
           (cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9] [0-9][0-9] [0-9][0-9] " " " human-readable)))
      (setq cleaned (replace-regexp-in-string " [0-9][0-9][0-9][0-9][0-9]+ " " " cleaned))
      (setq cleaned (org-trim cleaned))
      (capitalize cleaned))))

(defun org-astro--path-to-var-name (path)
  "Convert a file PATH to a camelCase JS variable name.
If the generated name starts with a number, it is prefixed with 'img'."
  (when (stringp path)
    (let* ((original-filename (file-name-sans-extension (file-name-nondirectory path)))
           ;; Use the sanitized filename for variable generation
           (clean-filename (org-astro--sanitize-filename original-filename))
           ;; Remove periods, underscores and other invalid characters for JS variable names
           (js-safe-filename (replace-regexp-in-string "[^a-zA-Z0-9-]" "" clean-filename))
           ;; Split on hyphens for camelCase conversion
           (parts (split-string js-safe-filename "[-]"))
           ;; Filter out empty parts
           (non-empty-parts (seq-filter (lambda (s) (> (length s) 0)) parts))
           (var-name (if (null non-empty-parts)
                         "image"
                         (concat (downcase (car non-empty-parts))
                                 (mapconcat #'capitalize (cdr non-empty-parts) "")))))
      ;; Ensure it starts with a letter (prefix with 'img' if starts with number)
      (if (and (> (length var-name) 0) (string-match-p "^[0-9]" var-name))
          (concat "img" var-name)
          var-name))))

(defun org-astro--get-task-nesting-level (heading)
  "Calculate nesting level for a TODO task by counting TODO ancestors."
  (let ((level 0)
        (current heading))
    (while (setq current (org-element-parent current))
      (when (and (eq (org-element-type current) 'headline)
                 (org-element-property :todo-keyword current))
        (setq level (1+ level))))
    level))

(defun org-astro--gen-yaml-front-matter (data)
  "Generate a YAML front-matter string from an alist DATA."
  (if (null data)
      ""
      (let ((yaml-str "---\n"))
        (dolist (pair data)
          (let ((key (car pair))
                (val (cdr pair)))
            (when val
              (setq yaml-str
                    (concat yaml-str
                            (format "%s: " (symbol-name key))
                            (if (listp val)
                                (concat "\n"
                                        (mapconcat (lambda (item)
                                                     (format "- %s" item))
                                                   val "\n")
                                        "\n")
                                (format "%s\n"
                                        (if (and (stringp val)
                                                 (or (string-match-p ":" val)
                                                     (string-match-p "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$" val))
                                                 (not (eq key 'publishDate)))
                                            ;; Quote strings that contain ':' or look like dates (avoid YAML parse issues)
                                            (format "\"%s\""
                                                    (replace-regexp-in-string
                                                     "\"" "\\\\\"" val))
                                            val))))))))
        (concat yaml-str "---\n"))))

(defun org-astro--get-title (tree info)
  "Return a title string from TREE/INFO, never nil."
  (or
   ;; Explicit #+TITLE first
   (let ((kw (org-element-map tree 'keyword
               (lambda (k)
                 (when (string-equal "TITLE" (org-element-property :key k)) k))
               nil 'first-match)))
     (when kw (org-element-property :value kw)))
   ;; Else first headline, safely exported
   (let ((hl (org-element-map tree 'headline 'identity nil 'first-match)))
     (when hl
       (org-astro--safe-export (org-element-property :title hl) info)))
   ;; Fallback
   "Untitled Post"))

(defun org-astro--get-excerpt (tree info)
  "Return an excerpt string from TREE/INFO, possibly empty but never nil.
Treats DESCRIPTION as a synonym for EXCERPT when EXCERPT is not present."
  (or
   ;; Prefer explicit #+ASTRO_EXCERPT or #+EXCERPT first
   (let ((kw (org-element-map tree 'keyword
               (lambda (k)
                 (when (member (org-element-property :key k)
                               '("ASTRO_EXCERPT" "EXCERPT"))
                   k))
               nil 'first-match)))
     (when kw
       (let* ((v (org-element-property :value kw)))
         (string-trim (replace-regexp-in-string "[*_/]" "" v)))))
   ;; Fall back to #+DESCRIPTION if no EXCERPT
   (let ((kw (org-element-map tree 'keyword
               (lambda (k)
                 (when (string= (org-element-property :key k) "DESCRIPTION")
                   k))
               nil 'first-match)))
     (when kw
       (let* ((v (org-element-property :value kw)))
         (string-trim (replace-regexp-in-string "[*_/]" "" v)))))
   ;; Else first paragraph, safely exported and cleaned
   (let ((p (org-element-map tree 'paragraph 'identity nil 'first-match)))
     (when p
       (let* ((raw (org-astro--safe-export (org-element-contents p) info))
              (clean (replace-regexp-in-string "[*_/]" "" raw))
              ;; Remove image tags like ![img](path) and <img...> tags
              (no-images (replace-regexp-in-string "!\\[.*?\\]([^)]*)" "" clean))
              (no-html-images (replace-regexp-in-string "<img[^>]*>" "" no-images))
              (one   (replace-regexp-in-string "\n" " " no-html-images)))
         ;; take first sentence up to ~300 chars, else truncate with ellipsis
         (if (string-match "\`\(.\{1,300\}?[.?!]\)" one)
             (org-trim (match-string 1 one))
             (truncate-string-to-width (org-trim one) 300 nil nil "…")))))
   ;; If all else fails:
   ""))



(defun org-astro--get-publish-date (info)
  "Extract and format the publish date from INFO.
  Falls back to the current time if no date is specified."
  (let ((date-raw (or (plist-get info :astro-publish-date)
                      (plist-get info :publish-date)
                      (plist-get info :date))))
    (if date-raw
        (org-astro--format-date date-raw info)
        (format-time-string (plist-get info :astro-date-format) (current-time)))))

(defun org-astro--get-author-image (info posts-folder)
  "Get the author image path from INFO, with defaults."
  (let ((author-image-raw (or (plist-get info :astro-author-image)
                              (plist-get info :author-image))))
    (or (and author-image-raw posts-folder
             (org-astro--process-image-path author-image-raw posts-folder "authors/"))
        org-astro-default-author-image)))

(defun org-astro--get-cover-image (info posts-folder)
  "Get the cover image path and alt text from INFO.
  If no explicit cover image is specified, use the first body image as hero."
  (let* ((image-raw (or (plist-get info :astro-image)
                        (plist-get info :cover-image)))
         (image (and image-raw posts-folder
                     (let ((slug-path (if slug (concat "posts/" slug "/") "posts/")))
                       (org-astro--process-image-path image-raw posts-folder slug-path t))))
         ;; If no explicit image, try to use first body image as hero
         (body-images (or (plist-get info :astro-body-images-imports)
                          org-astro--current-body-images-imports))
         (fallback-image (when (and (not image) body-images)
                           (plist-get (car body-images) :astro-path)))
         (final-image (or image fallback-image))
         (image-alt (or (plist-get info :astro-image-alt)
                        (plist-get info :cover-image-alt)
                        (and final-image (org-astro--filename-to-alt-text final-image)))))
    (list final-image image-alt)))

(defun org-astro--get-front-matter-data (tree info)
  "Build an alist of final front-matter data, applying defaults."
  (let* ((posts-folder (or (plist-get info :destination-folder)
                           (plist-get info :astro-posts-folder)))
         (title (org-astro--get-title tree info))
         ;; Always use slug from info or generate from title
         (slug (or (plist-get info :slug)
                   (when title (org-astro--slugify title))))
         (author (or (plist-get info :author) "Jay Dixit"))
         (excerpt (org-astro--get-excerpt tree info))
         (publish-date (org-astro--get-publish-date info))
         (date-occurred (org-astro--get-date-occurred tree info))
         (tags (org-astro--parse-tags tree info))
         (categories (org-astro--parse-categories tree info))
         (author-image (org-astro--get-author-image info posts-folder))
         (cover-image-data (org-astro--get-cover-image info posts-folder))
         (image (car cover-image-data))
         (image-alt (cadr cover-image-data))
         (era (org-astro--get-era tree info))
         (place (org-astro--get-place tree info))
         (places (org-astro--parse-places tree info))
         (themes-list (org-astro--parse-themes tree info))
         (people (org-astro--parse-people tree info))
         (emotions (org-astro--parse-emotions tree info))
         (story-type (org-astro--get-story-type tree info))
         (visibility (let ((v (plist-get info :visibility)))
                       (when (and v (not (string-empty-p (org-trim v))))
                         (org-trim v))))
         (theme (let ((th (plist-get info :theme)))
                  (when (and th (not (string-empty-p (org-trim th))))
                    (org-trim th))))
         (status (plist-get info :status))
         (draft (when (and status (string= (downcase (org-trim status)) "draft")) "true")))
    ;; Return the alist of final data - include visibility as a string when provided
    `((title . ,title)
      ,@(when slug `((slug . ,slug)))
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      ,@(when date-occurred `((dateOccurred . ,date-occurred)))
      (tags . ,tags)
      ,@(when place `((place . ,place)))
      ,@(when people `((people . ,people)))
      ,@(when emotions `((emotions . ,emotions)))
      ,@(when places `((places . ,places)))
      ,@(when themes-list `((themes . ,themes-list)))
      ,@(when story-type `((storyType . ,story-type)))
      (categories . ,categories)
      ,@(when era `((era . ,era)))
      ,@(when visibility `((visibility . ,visibility)))
      ,@(when theme `((theme . ,theme)))
      ,@(when draft `((draft . ,draft))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX, robust to partial INFO during preflight."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link)))
    (cond
     ;; org-roam ID links → resolve to relative Markdown links
     ((string= type "id")
      (let* ((target-id path)
             (entry (and org-astro--id-path-map
                         (gethash target-id org-astro--id-path-map)))
             (link-text (if (and desc (not (string-blank-p desc)))
                            desc
                          (or (and entry (plist-get entry :title))
                              target-id))))
        (if (and entry org-astro--current-outfile)
            (let* ((target-outfile (or (plist-get entry :outfile)
                                       (let* ((posts-folder (plist-get entry :posts-folder))
                                              (filename (plist-get entry :filename))
                                              (relative-dir (plist-get entry :relative-subdir)))
                                         (when (and posts-folder filename)
                                           (let ((base (if (and relative-dir (not (string-blank-p relative-dir)))
                                                           (expand-file-name relative-dir posts-folder)
                                                         posts-folder)))
                                             (expand-file-name filename base)))))))
              (if (and target-outfile)
                  (let ((relative (org-astro--calculate-relative-mdx-path
                                   org-astro--current-outfile target-outfile)))
                    (if relative
                        (format "[%s](%s)" link-text relative)
                      (org-astro--record-missing-id-link info target-id link-text)))
                (org-astro--record-missing-id-link info target-id link-text)))
          (org-astro--record-missing-id-link info target-id link-text))))
     ;; Local file image links → render Image component using imports
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (or (string-match-p "\\.\\(png\\|jpe?g\\|webp\\)$" path)
               (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" path)))
      (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                    org-astro--current-body-images-imports))
             (explicit-hero (or (plist-get info :astro-image)
                                (plist-get info :cover-image)))
             (image-imports image-imports-raw)
             (_ (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                  (message "[ox-astro][img] LINK processing path=%s, imports count=%d"
                           path (length image-imports-raw))))
             (image-data
              (when image-imports
                (or (cl-find path image-imports
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal)
                    ;; Fallback: match by sanitized filename if absolute path compare fails
                    (let* ((base (file-name-nondirectory path))
                           (base-s (org-astro--sanitize-filename (file-name-sans-extension base))))
                      (cl-find base-s image-imports
                               :key (lambda (item)
                                      (org-astro--sanitize-filename
                                       (file-name-sans-extension
                                        (file-name-nondirectory (plist-get item :path)))))
                               :test #'string-equal))))))
        (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
          (message "[ox-astro][img] LINK path=%s explicit-hero=%s imports=%s match=%s"
                   path (and explicit-hero t) (length image-imports)
                   (and image-data (plist-get image-data :var-name))))
        (cond
         ;; Suppress only the very first occurrence of the implicit hero at top
         ((and (not explicit-hero)
               image-imports-raw
               (string-equal path (plist-get (car image-imports-raw) :path))
               (not (plist-get info :astro-hero-skipped-top)))
          (plist-put info :astro-hero-skipped-top t)
          "")
         (image-data
          (let* ((var-name (plist-get image-data :var-name))
                 (alt-text (or desc (org-astro--filename-to-alt-text path) "Image")))
            (format "<Image src={%s} alt=\"%s\" />" var-name alt-text)))
         ;; Unmatched: fall back to normal Markdown link instead of dropping
         (t
          (let ((md (when (fboundp 'org-md-link)
                      (ignore-errors (org-md-link link desc info)))))
            (or md (or (org-element-property :raw-link link)
                       (concat (or type "file") ":" path))))))))
     ;; PDF links → emit a Markdown link with URL-encoded spaces; normalize label.
     ((and path
           (or (string= type "file") (and (null type) (string-prefix-p "/" path)))
           (string-match-p "\\.pdf\\(\\?\\|#\\|$\\)" path))
      (let* (;; If this is an absolute local file path under a public/pdfs directory,
             ;; rewrite it to a site path beginning with /pdfs/.
             (site-path
              (cond
               ;; Already a site path
               ((string-prefix-p "/pdfs/" path) 
                path)
               ;; Local absolute file path: try to detect /public/pdfs/ segment
               ((and (string-prefix-p "/" path)
                     (string-match "/public/pdfs/\\(.*\\)$" path))
                (concat "/pdfs/" (match-string 1 path)))
               (t 
                path)))
             (encoded (if (string-match-p " " site-path)
                          (replace-regexp-in-string " " "%20" site-path)
                        site-path))
             (label-raw (or desc (file-name-base path)))
             ;; Drop any leading symbols like ▌ and normalize "PDF:" spacing
             (label-1 (replace-regexp-in-string "^\\s-*[▌│•▪️]+" "" label-raw))
             (label-a (replace-regexp-in-string "PDF:" "PDF: " label-1))
             (label   (replace-regexp-in-string "PDF:  +" "PDF: " label-a)))
        (format "[%s](%s)" label encoded)))

     ;; If the description is already a Markdown link, preserve it unchanged.
     ((and desc (org-astro--contains-markdown-link-p desc))
      desc)
     ;; Internal target (fuzzy) → local anchor; degrade gracefully if INFO is partial
     ((and (string= type "fuzzy") (not (string-match-p "://" path)))
      (let* ((resolved-title
              (condition-case _
                  (let* ((target (and (plist-get info :parse-tree)
                                      (org-export-resolve-fuzzy-link link info))))
                    (and target (org-element-property :raw-value target)))
                (error nil)))
             (text (or desc resolved-title path))
             (slug (org-astro--slugify text)))
            (format "[%s](#%s)" text slug)))

     ;; Bare URLs with no description → LinkPeek + set import flag
     ((and (null desc) (member type '("http" "https" "ftp" "mailto")))
      ;; If this bare URL is part of a Markdown reference link definition
      ;; like "[1]: https://example.com", preserve it literally.
      (let* ((parent (condition-case _
                         (org-element-property :parent link)
                       (error nil))))
        (if (and parent (org-astro--markdown-link-definition-paragraph-p parent))
            ;; Return the raw URL so the whole line exports as "[x]: url"
            (or (org-element-property :raw-link link)
                (concat type ":" path))
            ;; Otherwise, emit LinkPeek + mark for import
            (progn
              (setf (plist-get info :astro-uses-linkpeek) t)
              (format "<LinkPeek href=\"%s\"></LinkPeek>"
                      (or (org-element-property :raw-link link)
                          (concat type ":" path)))))))

     ;; Everything else → prefer org's MD translator; fall back to plain Markdown
     (t
      (let ((md (when (fboundp 'org-md-link)
                  (ignore-errors (org-md-link link desc info)))))
        (or md
            (let* ((raw (or (org-element-property :raw-link link)
                            (concat type ":" path)))
                   (text (or desc raw)))
              (format "[%s](%s)" text raw))))))))

(defun org-astro-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element into fenced Markdown format.
For 'user', 'prompt', and 'quote' blocks, preserve org-mode syntax
literally - convert org headings to markdown equivalents."
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        ;; For user/prompt/quote blocks, convert org-mode syntax to markdown
        (when (member lang '("user" "prompt" "quote"))
          ;; Convert em dashes
          (when (string-match-p "---" code)
            (setq code (replace-regexp-in-string "---" "—" code)))
          ;; Convert org headings to markdown headings
          (setq code (replace-regexp-in-string "^\\*\\*\\*\\* \\(.*\\)$" "#### \\1" code))
          (setq code (replace-regexp-in-string "^\\*\\*\\* \\(.*\\)$" "### \\1" code))
          (setq code (replace-regexp-in-string "^\\*\\* \\(.*\\)$" "## \\1" code))
          (setq code (replace-regexp-in-string "^\\* \\(.*\\)$" "# \\1" code)))
        ;; Trim trailing newlines/whitespace to prevent extra space at the end.
        (setq code (string-trim-right code))
        (format "```%s\n%s\n```" (or lang "") code))
      ;; Fallback to simple fenced code block
      (let* ((lang (org-element-property :language src-block))
             (code (org-element-property :value src-block)))
        (format "```%s\n%s\n```" (or lang "") (org-trim code)))))

(defun org-astro-heading (heading contents info)
  (let ((todo-keyword (org-element-property :todo-keyword heading)))
    (if todo-keyword
        ;; task style
        (let* ((info-copy (copy-sequence info))
               (_ (plist-put info-copy :with-smart-quotes nil))
               (title (org-astro--safe-export (org-element-property :title heading) info-copy))
               (nesting-level (org-astro--get-task-nesting-level heading))
               (indent (make-string (* 2 nesting-level) ? ))
               (donep (member todo-keyword org-done-keywords))
               (checkbox (if donep "[x]" "[ ]"))
               (trimmed-contents (if contents (org-trim contents) ""))
               (indented-contents
                (if (> (length trimmed-contents) 0)
                    (let ((content-indent (make-string (+ 2 (* 2 nesting-level)) ? )))
                      (concat "\n" content-indent
                              (replace-regexp-in-string
                               "\n"
                               (concat "\n" content-indent)
                               trimmed-contents)))
                    "")))
          (format "%s- %s %s%s\n" indent checkbox title indented-contents))
        ;; regular heading
        (let* ((info-copy (copy-sequence info))
               (_ (plist-put info-copy :with-smart-quotes nil))
               (title (org-astro--safe-export (org-element-property :title heading) info-copy))
               (level (+ (org-element-property :level heading)
                         (or (plist-get info :headline-offset) 0)))
               (level (min (max level 1) 6))
               (header (concat (make-string level ?#) " " title)))
          (concat header "\n\n" (or contents ""))))))

(defun org-astro--handle-broken-image-paragraph (paragraph info)
  "Handle a paragraph containing a broken image path with subscripts."
  (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                org-astro--current-body-images-imports))
         ;; Exclude first image if it's being used as the implicit hero
         (explicit-hero (or (plist-get info :astro-image)
                            (plist-get info :cover-image)))
         (image-imports (if (and (not explicit-hero) image-imports-raw)
                            (cdr image-imports-raw)
                            image-imports-raw))
         (paragraph-context (org-element-interpret-data paragraph))
         (matching-import nil))


    ;; Try to find a matching imported image by comparing filenames
    (when image-imports
      (dolist (import image-imports)
        (let* ((import-file-path (plist-get import :path))
               (import-filename (file-name-nondirectory import-file-path)))
          ;; Check if the paragraph context contains this filename (even broken up)
          (when (and import-filename
                     (string-match-p (regexp-quote (file-name-sans-extension import-filename))
                                     paragraph-context))
            (setq matching-import import)))))

    (if matching-import
        ;; Generate Image component for matched import
        (let* ((var-name (plist-get matching-import :var-name))
               (matched-path (plist-get matching-import :path))
               (alt-text (or (org-astro--filename-to-alt-text matched-path) "Image")))
          (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
        ;; No match found - remove the broken paragraph
        "")))


(defun org-astro-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element.
  If the paragraph is a raw image path, convert it to an <img> tag.
  Otherwise, use the default Markdown paragraph transcoding."
  ;; Debug: Log paragraphs that might contain raw image paths
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((children (org-element-contents paragraph))
           (child (and (= 1 (length children)) (car children))))
      (when (and child (eq 'plain-text (org-element-type child)))
        (let* ((raw-text (org-element-property :value child))
               (text (when (stringp raw-text) (org-trim raw-text))))
          (when (and text (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text))
            (org-astro--dbg-log info "PARAGRAPH processing raw image path: %s" text))))))
  (let* ((children (org-element-contents paragraph))
         (child (and (= 1 (length children)) (car children)))
         (is-image-path nil)
         path)
    (when (and child (eq 'plain-text (org-element-type child)))
      (let* ((raw-text (org-element-property :value child))
             (text (when (stringp raw-text) (org-trim raw-text))))
        (when (and text
                   (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text))
          (setq is-image-path t)
          (setq path text))))
    (if is-image-path
        (let* ((image-imports-raw (or (plist-get info :astro-body-images-imports)
                                      org-astro--current-body-images-imports))
               ;; Exclude first image if it's being used as the implicit hero
               (explicit-hero (or (plist-get info :astro-image)
                                  (plist-get info :cover-image)))
               (image-imports (if (and (not explicit-hero) image-imports-raw)
                                  (cdr image-imports-raw)
                                  image-imports-raw))
               (image-data (when image-imports
                             (or (cl-find path image-imports
                                          :key (lambda (item) (plist-get item :path))
                                          :test #'string-equal)
                                 ;; Fallback by sanitized filename (handles underscore→hyphen sanitation)
                                 (let* ((base (file-name-nondirectory path))
                                        (base-s (org-astro--sanitize-filename (file-name-sans-extension base))))
                                   (cl-find base-s image-imports
                                            :key (lambda (item)
                                                   (org-astro--sanitize-filename
                                                    (file-name-sans-extension
                                                     (file-name-nondirectory (plist-get item :path)))))
                                            :test #'string-equal))))))
          (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
            (message "[ox-astro][img] PARA path=%s explicit-hero=%s imports=%s match=%s"
                     path (and explicit-hero t) (length image-imports)
                     (and image-data (plist-get image-data :var-name))))
          (if image-data
              (let ((var-name (plist-get image-data :var-name))
                    (alt-text (or (org-astro--filename-to-alt-text path) "Image")))
                (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
              ;; Fallback: if image wasn't processed by the filter, just output the original contents.
              contents))
        ;; Check if this paragraph contains broken image path (subscripts)
        (let ((paragraph-context (org-element-interpret-data paragraph)))
          (if (string-match-p "/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)" paragraph-context)
              ;; This paragraph contains a broken image path - try to handle it
              (org-astro--handle-broken-image-paragraph paragraph info)
              ;; Regular paragraph
              (org-md-paragraph paragraph contents info))))))


(defun org-astro-plain-text (text info)
  "Transcode a plain-text element.
  If the text contains raw image paths on their own lines, convert them to <img> tags.
  If the text contains raw URLs on their own lines, convert them to LinkPeek components."
  (let* ((lines (split-string text "\n"))
         (image-imports (or (plist-get info :astro-body-images-imports)
                            org-astro--current-body-images-imports))
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line)))
               (if (org-astro--contains-markdown-link-p line)
                   ;; Preserve Markdown link formatting unchanged
                   line
                   (cond
                    ;; Raw image path (trust imports rather than filesystem)
                    ((and trimmed-line
                          (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" trimmed-line)
                              (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" trimmed-line)))
                     (let ((image-data (when image-imports
                                         (cl-find trimmed-line image-imports
                                                  :key (lambda (item) (plist-get item :path))
                                                  :test #'string-equal))))
                       (if image-data
                           (let ((var-name (plist-get image-data :var-name))
                                 (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                             (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                           ;; Fallback: if image wasn't processed by the filter, output as plain text.
                           line)))
                    ;; Remote image URL
                    ((and trimmed-line
                          (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" trimmed-line))
                     (let ((image-data (when image-imports
                                         (cl-find trimmed-line image-imports
                                                  :key (lambda (item) (plist-get item :path))
                                                  :test #'string-equal))))
                       (if image-data
                           (let ((var-name (plist-get image-data :var-name))
                                 (alt-text (or (org-astro--filename-to-alt-text trimmed-line) "Image")))
                             (format "<Image src={%s} alt=\"%s\" />" var-name alt-text))
                           ;; Fallback: if image wasn't processed by the filter, output as plain text.
                           line)))
                    ;; Regular remote URL (non-image) is now handled correctly by org-astro-link.
                    ;; Regular line
                    (t line)))))
           lines)))
    ;; Store LinkPeek usage in info for import generation
    (when has-linkpeek
      (plist-put info :astro-uses-linkpeek t))
    (mapconcat 'identity processed-lines "\n")))


(defun org-astro-subscript (subscript contents info)
  "Handle subscript elements, removing ones that are part of broken image paths."
  (let* ((parent (org-element-property :parent subscript))
         (parent-context (when parent (org-element-interpret-data parent))))
    (if (and parent-context
             (string-match-p "/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)" parent-context))
        ;; This subscript is part of a broken image path - remove it entirely
        ;; The raw image collection will handle the proper image processing
        ""
        ;; Regular subscript - use default markdown handling
        (format "_%s_" (or contents "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Export Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro--collect-images-from-tree (tree)
  "Collect all image paths from the parse TREE.
  This includes both `[[file:...]]` links and raw image paths on their own line."
  (let (images)
    ;; 1. Collect from `link` elements
    (org-element-map tree 'link
      (lambda (link)
        (let ((type (org-element-property :type link))
              (path (org-element-property :path link)))
          (when (and (string= type "file")
                     (string-match-p "\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)$" path))
            (push path images)))))
    ;; 2. Collect from raw paths in all plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element)
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n"))))
          (dolist (line lines)
            (let ((text (org-trim line)))
              (when (and text
                         ;; Match absolute paths OR paths containing "assets/images"
                         (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" text)
                             (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" text)))
                (push text images)))))))
    ;; 3. Collect from paragraphs that contain subscript elements (broken up image paths)
    (org-element-map tree 'paragraph
      (lambda (paragraph)
        (let ((reconstructed-path (org-astro--extract-image-path-from-paragraph paragraph)))
          (when (and reconstructed-path
                     (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" reconstructed-path)
                         (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" reconstructed-path)))
            (push reconstructed-path images)))))
    ;; 4. Collect remote image URLs from plain-text elements
    (org-element-map tree 'plain-text
      (lambda (text-element)
        (let* ((raw-text (org-element-property :value text-element))
               (lines (when (stringp raw-text) (split-string raw-text "\n"))))
          (dolist (line lines)
            (let ((text (org-trim line)))
              (when (and text
                         (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" text))
                (push text images)))))))
    ;; Return a list with no duplicates
    (delete-dups (nreverse images))))

(defun org-astro--collect-raw-image-paths ()
  "Collect image paths from raw buffer content, before org-mode parsing.
  This catches paths with underscores that would be broken by subscript parsing."
  (let (images)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
        (let ((path (string-trim (match-string 0))))
          (push path images))))
    images))

(defun org-astro--extract-image-path-from-paragraph (paragraph)
  "Extract a potential image path from a PARAGRAPH that may contain subscript elements."
  (let ((raw-content (org-element-interpret-data paragraph)))
    ;; Look for patterns like /Users/jay/Downloads/file_name.webp that may have been broken by subscripts
    (when (string-match "/[^[:space:]<>]*\\.\\(webp\\|png\\|jpe?g\\)" raw-content)
      (let ((potential-path (match-string 0 raw-content)))
        ;; Clean up any HTML artifacts
        (setq potential-path (replace-regexp-in-string "<[^>]*>" "" potential-path))
        ;; Clean up any whitespace
        (setq potential-path (string-trim potential-path))
        ;; If it looks like a valid absolute path, return it
        (when (string-match-p "^/" potential-path)
          potential-path)))))



;;;; Export Block Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-export-block (export-block _contents info)
  "Transcode EXPORT-BLOCK element into MDX format.
Recognizes MDX, MARKDOWN, and MD export blocks and passes their content through verbatim."
  (let ((type (org-element-property :type export-block)))
    (if (member (upcase type) '("MDX" "MARKDOWN" "MD"))
        ;; Pass through the content completely unindented to prevent the final-output
        ;; filter from converting indented lines to blockquotes
        (let ((content (org-element-property :value export-block)))
          (replace-regexp-in-string "^[ \t]+" "" content))
      ;; For other types, fall back to HTML backend
      (org-export-with-backend 'html export-block nil info))))

(require 'ox-astro-metadata)

(provide 'ox-astro-helpers)
;;; ox-astro-helpers.el ends here
