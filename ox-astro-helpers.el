;;; ox-astro-helpers.el --- Helper functions for ox-astro  -*- lexical-binding: t -*-

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'subr-x) ; for string-trim, string-trim-right
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'org-id)

;; Fallback for older Org that might not provide org-element-parent.
(unless (fboundp 'org-element-parent)
  (defun org-element-parent (element)
    "Return parent of ELEMENT via :parent property (compat shim)."
    (org-element-property :parent element)))

(defun org-astro--normalize-image-key (path &optional source-file)
  "Normalize PATH to a key for image metadata lookup.
Prefer a relative path (from `org-astro-source-root-folder' when available) to
avoid filename collisions. Fall back to the normalized filename."
  (when path
    (let* ((abs-path
            (cond
             ((string-match-p "\\`https?://" path) path)
             ((string-prefix-p "//" path) (concat "https:" path))
             ((file-name-absolute-p path) (expand-file-name path))
             ((and source-file (file-name-absolute-p source-file))
              (expand-file-name path (file-name-directory source-file)))
             (t path)))
           (root (and (boundp 'org-astro-source-root-folder)
                      org-astro-source-root-folder
                      (file-name-absolute-p org-astro-source-root-folder)
                      (expand-file-name org-astro-source-root-folder)))
           (source-dir (when (and source-file (file-name-absolute-p source-file))
                         (file-name-directory source-file)))
           (relative (when (and root abs-path (file-name-absolute-p abs-path)
                                (string-prefix-p root (expand-file-name abs-path)))
                       (file-relative-name abs-path root)))
           (relative-from-source (when (and source-dir abs-path (file-name-absolute-p abs-path)
                                            (string-prefix-p source-dir (expand-file-name abs-path)))
                                   (file-relative-name abs-path source-dir))))
      (cond
       ;; Keep remote URLs as-is (case sensitive), but normalize underscores.
       ((and abs-path (string-match-p "\\`https?://" abs-path))
        (replace-regexp-in-string "_" "-" abs-path))
       ;; Use normalized relative path when possible.
       (relative
        (let ((clean (replace-regexp-in-string "_" "-" (downcase relative))))
          (replace-regexp-in-string "//+" "/" clean)))
       ;; Fallback to relative path from the source file directory if available.
       (relative-from-source
        (let ((clean (replace-regexp-in-string "_" "-" (downcase relative-from-source))))
          (replace-regexp-in-string "//+" "/" clean)))
       ;; Fall back to normalized filename (lowercase + underscores → hyphens).
       (t
        (let* ((filename (file-name-nondirectory abs-path))
               (base (file-name-sans-extension filename))
               (ext (file-name-extension filename)))
          (concat (downcase (replace-regexp-in-string "_" "-" base))
                  (when ext (concat "." (downcase ext))))))))))

(defvar org-astro-known-posts-folders nil
  "List of destination folders for exports, each entry a (NICKNAME . PLIST).")

;; Silence org-id scan chatter during batch exports.
(when (fboundp 'org-id-locations--scan)
  (advice-add 'org-id-locations--scan :around
              (lambda (orig-fn &rest args)
                (let ((inhibit-message t)
                      (message-log-max nil))
                  (apply orig-fn args)))))

(when (fboundp 'org-id-locations--message)
  (advice-add 'org-id-locations--message :around
              (lambda (orig-fn &rest args)
                (let ((inhibit-message t)
                      (message-log-max nil))
                  (apply orig-fn args)))))

;; Silence org-id-locations-update progress and force silent flag.
(when (fboundp 'org-id-locations-update)
  (advice-add 'org-id-locations-update :around
              (lambda (orig-fn &rest args)
                (let* ((inhibit-message t)
                       (message-log-max nil)
                       (args (if (>= (length args) 2)
                                 (progn (setcar (nthcdr 1 args) t) args)
                               (append args (list t)))))
                  (apply orig-fn args)))))

;; Compatibility helpers for older Emacs builds.
(unless (fboundp 'cl-putf)
  (defun cl-putf (plist indicator value &rest pairs)
    "Fallback implementation of `cl-putf' for older Emacs.
PLIST is the property list to update.  INDICATOR and VALUE define
the first key/value pair to store, and PAIRS can supply additional
indicator/value pairs.  Returns the updated plist."
    (let ((result plist)
          (key indicator)
          (val value)
          (rest pairs))
      (while key
        (setq result (plist-put result key val))
        (setq key (car rest))
        (setq rest (cdr rest))
        (when key
          (setq val (car rest))
          (setq rest (cdr rest))))
      result)))

;; Declare functions from other ox-astro modules
(declare-function org-astro--process-image-path "ox-astro-image-handlers")
(declare-function org-astro--process-pdf-path "ox-astro-pdf-handlers")
(declare-function org-astro--collect-pdfs-from-tree "ox-astro-pdf-handlers")
(declare-function org-astro--build-image-manifest "ox-astro-image-handlers")
(declare-function org-astro--collect-raw-images-from-tree-region "ox-astro-image-handlers")
(declare-function org-astro--image-remote-p "ox-astro-image-handlers")
(declare-function org-astro--sanitize-filename "ox-astro-image-handlers")
(declare-function org-astro--build-render-map "ox-astro-image-handlers")
(declare-function org-astro--image-entry-alt "ox-astro-image-handlers")
(declare-function org-astro--update-source-buffer-image-path "ox-astro-image-handlers")
(declare-function org-astro--clean-tag "ox-astro-metadata")
(declare-function org-astro--parse-tags "ox-astro-metadata")
(declare-function org-astro--parse-categories "ox-astro-metadata")
(declare-function org-astro--parse-places "ox-astro-metadata")
(declare-function org-astro--parse-people "ox-astro-metadata")
(declare-function org-astro--parse-emotions "ox-astro-metadata")
(declare-function org-astro--parse-themes "ox-astro-metadata")
(declare-function org-astro--parse-media "ox-astro-metadata")
(declare-function org-astro--parse-incomplete "ox-astro-metadata")
(declare-function org-astro--parse-connections "ox-astro-metadata")
(declare-function org-astro--get-date-occurred "ox-astro-metadata")
(declare-function org-astro--get-era "ox-astro-metadata")
(declare-function org-astro--get-place "ox-astro-metadata")
(declare-function org-astro--get-story-type "ox-astro-metadata")
(declare-function org-astro--get-hero-credit "ox-astro-metadata")
(declare-function org-astro--get-hero-caption "ox-astro-metadata")

;; Declare global variable for data persistence across export phases
(defvar org-astro--current-body-images-imports nil
  "Global storage for body image imports to persist across export phases.")

(defvar org-astro--id-path-map nil
  "Hash table mapping org-roam IDs to export metadata for the current run.")

(defvar org-astro--broken-link-accumulator nil
  "Hash table mapping output files to unresolved org-roam ID links.")

(defvar org-astro--broken-link-warnings-issued nil
  "Hash table used to deduplicate warnings for missing org-roam IDs.")

(defvar org-astro--duplicate-ids-logged nil
  "When non-nil, duplicate ID summary has been logged for this batch.")

(defvar org-astro--current-outfile nil
  "Absolute path to the MDX file currently being generated.")

(defvar org-astro--current-output-root nil
  "Root output directory for the current export destination.")

(defvar org-astro--current-id-link-base-path nil
  "Base path for absolute ID link routes during current export.
When non-nil, ID links render as /base-path/collection-id instead of relative .mdx paths.
Set from :id-link-base-path in destination config or global `org-astro-id-link-base-path'.")

(defun org-astro--string-truthy-p (value)
  "Return non-nil when VALUE (a string) looks truthy.
Accepts t/true/yes/y/on/1 (case-insensitive)."
  (let* ((val (downcase (string-trim (or value "")))))
    (member val '("t" "true" "yes" "y" "on" "1"))))

(defun org-astro--youtube-id-from-url (url)
  "Extract a YouTube video ID from URL or return nil.
Supports youtu.be short links, watch URLs, embed URLs, or bare 11-character IDs."
  (let ((s (string-trim (or url ""))))
    (cond
     ((string-match "youtu\.be/\\([^?&#/]+\\)" s)
      (match-string 1 s))
     ((string-match "youtube\.com/watch[^?]*[?&]v=\\([^&?#/]+\\)" s)
      (match-string 1 s))
     ((string-match "youtube\.com/embed/\\([^?&#/]+\\)" s)
      (match-string 1 s))
     ((string-match "^[A-Za-z0-9_-]\{11\}$" s)
      s)
     (t nil))))

(defun org-astro--youtube-embed (video-id)
  "Return responsive iframe markup for VIDEO-ID."
  (let ((escaped-id (org-astro--escape-attribute video-id)))
    (format (concat
             "<div class=\"astro-youtube-embed\" style=\"position: relative; padding-bottom: 56.25%%; height: 0; overflow: hidden; max-width: 100%%; margin: 2rem 0;\">\n"
             "  <iframe style=\"position: absolute; top: 0; left: 0; width: 100%%; height: 100%%;\"\n"
             "          src=\"https://www.youtube.com/embed/%s\"\n"
             "          title=\"YouTube video\"\n"
             "          frameborder=\"0\"\n"
             "          allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\"\n"
             "          allowfullscreen></iframe>\n"
             "</div>\n")
            escaped-id)))

(defun org-astro--escape-attribute (text)
  "Escape TEXT for safe use inside double-quoted JSX/HTML attributes.
Escapes &, <, >, and \" to their entity equivalents."
  (let ((s (or text "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    (replace-regexp-in-string "\"" "&quot;" s)))

(defun org-astro--image-dimensions (path)
  "Return (WIDTH . HEIGHT) for PATH when readable; nil otherwise."
  (when (and path (file-exists-p path) (not (org-astro--image-remote-p path)))
    (condition-case _
        (let* ((type (image-type-from-file-name path))
               (image (and type (create-image path type)))
               (size (and image (image-size image t))))
          (when (and size (consp size))
            (cons (round (car size)) (round (cdr size)))))
      (error nil))))

(defun org-astro--normalize-image-path (path)
  "Normalize PATH for reliable image comparisons."
  (cond
   ((null path) nil)
   ((string-match-p "\\`https?://" path) path)
   ((string-prefix-p "//" path) (concat "https:" path))
   ((string-prefix-p "~/" path) (expand-file-name path))
   ((file-name-absolute-p path) (expand-file-name path))
   (t path)))

(defun org-astro--same-image-path-p (path-a path-b)
  "Return non-nil when PATH-A and PATH-B refer to the same image."
  (let ((norm-a (org-astro--normalize-image-path path-a))
        (norm-b (org-astro--normalize-image-path path-b)))
    (and norm-a norm-b (string= norm-a norm-b))))

(defun org-astro--hero-image-entry-p (entry hero-path)
  "Return non-nil when ENTRY corresponds to HERO-PATH."
  (when (and entry hero-path)
    (let ((candidates (list (plist-get entry :astro-path)
                            (plist-get entry :path)
                            (plist-get entry :original-path))))
      (cl-some (lambda (candidate)
                 (org-astro--same-image-path-p candidate hero-path))
               candidates))))

(defun org-astro--format-image-component (var-name alt-text &optional credit caption width height)
  "Return a standardized Image component string for VAR-NAME and ALT-TEXT.
Includes layout prop based on `org-astro-image-default-layout' config.
If CREDIT or CAPTION is non-nil, wraps the image in a figure with PhotoSwipe support.
CREDIT appears on the page, CAPTION appears in lightbox.
WIDTH/HEIGHT (numbers) populate PhotoSwipe data attributes when available."
  (let* ((layout-prop (when (and (boundp 'org-astro-image-default-layout)
                                 org-astro-image-default-layout
                                 (not (string= org-astro-image-default-layout "none")))
                        (format " layout=\"%s\"" org-astro-image-default-layout)))
         (escaped-alt (org-astro--escape-attribute (or alt-text "Image")))
         (image-tag (format "<Image src={%s} alt=\"%s\"%s />"
                            var-name
                            escaped-alt
                            (or layout-prop "")))
         (width-attr (when width (format " data-pswp-width=\"%s\"" width)))
         (height-attr (when height (format " data-pswp-height=\"%s\"" height))))
    (if (or credit caption)
        ;; Wrap in figure with PhotoSwipe-compatible link and caption
        (let* ((lightbox-parts
                (delq nil
                      (list (when credit (format "<strong>%s</strong>" credit))
                            (when caption (format "<em>%s</em>" caption)))))
               (lightbox-caption (mapconcat #'identity lightbox-parts "<br/>"))
               (escaped-lightbox (org-astro--escape-attribute lightbox-caption))
               ;; Page caption shows credit only (caption/prompt shown in lightbox)
               (page-caption credit))
          (format "<figure class=\"image-figure\">\n<a href={%s.src} data-pswp-item=\"true\" data-pswp-caption=\"%s\"%s%s>\n%s\n</a>%s\n</figure>"
                  var-name
                  escaped-lightbox
                  (or width-attr "")
                  (or height-attr "")
                  image-tag
                  (if page-caption
                      (format "\n<figcaption class=\"image-caption text-xs text-gray-400 mt-2\">%s</figcaption>" page-caption)
                    "")))
      image-tag)))

(defun org-astro--image-component-for-record (record info &optional alt-override img-path)
  "Render RECORD as an Image component, skipping the hero's first inline usage.

INFO carries export state. When the record corresponds to the hero image and the
hero has not yet been suppressed in the body, return an empty string and mark it
as suppressed so later explicit uses still render. All other records return the
standard Image component string.
IMG-PATH is used to look up credit/caption metadata if provided."
  (when record
    (let* ((entry (plist-get record :entry))
           (var-name (plist-get record :var-name))
           (alt (or alt-override (plist-get record :alt)))
           (hero (plist-get info :astro-hero-image))
           (record-hero (plist-get record :hero))
           (already-suppressed (plist-get info :astro-hero-body-suppressed))
           (is-hero (or record-hero
                        (and hero entry (org-astro--hero-image-entry-p entry hero)))))
      (if (and is-hero (not already-suppressed))
          (progn
            (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
              (org-astro--debug-log info "Skipping inline hero image for %s" var-name))
            (setf (plist-get info :astro-hero-body-suppressed) t)
            ;; Hero suppressed - don't render anything (including credit/caption)
            "")
        ;; Non-hero (or already-suppressed hero): render with optional credit/caption.
        (let* ((credit-map (plist-get info :astro-image-credits))
               (caption-map (plist-get info :astro-image-captions))
               (norm-key (and img-path (org-astro--normalize-image-key img-path (plist-get info :input-file))))
               (credit (and credit-map norm-key (gethash norm-key credit-map)))
               (caption (and caption-map norm-key (gethash norm-key caption-map)))
               (target-path (or (plist-get record :target-path)
                                (plist-get record :path)
                                (plist-get entry :target-path)
                                (plist-get entry :path)))
               (dimensions (and target-path (org-astro--image-dimensions target-path)))
               (width (car dimensions))
               (height (cdr dimensions)))
          (when var-name
            (let ((current (plist-get info :astro-render-used-vars)))
              (setf (plist-get info :astro-render-used-vars)
                    (cl-adjoin var-name current :test #'equal))))
          (org-astro--format-image-component var-name alt credit caption width height))))))

(defun org-astro--lookup-render-record (path info)
  "Return render metadata for PATH from INFO's render map."
  (let* ((clean (and path (substring-no-properties path)))
         (render-map (plist-get info :astro-render-map))
         (processed (or (plist-get info :astro-body-images-imports)
                        org-astro--current-body-images-imports)))
    (when (and (null render-map) processed)
      (let* ((data (org-astro--build-render-map processed (plist-get info :astro-hero-image)))
             (render-map-data (plist-get data :map))
             (imports (plist-get data :imports)))
        (when render-map-data
          (setf (plist-get info :astro-render-map) render-map-data)
          (setq render-map render-map-data))
        (when imports
          (setf (plist-get info :astro-render-imports)
                (cl-remove-duplicates
                 (append imports (plist-get info :astro-render-imports))
                 :test #'equal)))))
    (when clean
      (or (and render-map (gethash clean render-map))
          (let ((sanitized (org-astro--sanitize-filename
                            (file-name-sans-extension
                             (file-name-nondirectory clean)))))
            (and render-map sanitized (gethash sanitized render-map)))
          (let* ((entry (and processed
                             (or (cl-find clean processed
                                          :key (lambda (item) (plist-get item :path))
                                          :test #'string-equal)
                                 (cl-find clean processed
                                          :key (lambda (item) (plist-get item :original-path))
                                          :test #'string-equal)))))
            (when entry
              (let* ((astro (plist-get entry :astro-path))
                     (var-name (plist-get entry :var-name))
                     (alt (org-astro--image-entry-alt entry)))
                (when (and astro var-name)
                  (list :entry entry
                        :var-name var-name
                        :astro-path astro
                        :alt alt
                        :jsx (org-astro--format-image-component var-name alt))))))))))

;; Simple debug logging function that writes directly to file
(defun org-astro--debug-log-direct (fmt &rest args)
  "Write debug message directly to log file when debugging is enabled."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
    (let* ((msg (apply #'format fmt args))
           (timestamp (format-time-string "%H:%M:%S"))
           (line (format "[%s] %s\n" timestamp msg))
           (debug-file (if (boundp 'org-astro-debug-log-file)
                           (expand-file-name org-astro-debug-log-file)
                           (expand-file-name "ox-astro-debug.log" temporary-file-directory))))
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
           (debug-file (if (boundp 'org-astro-debug-log-file)
                           (expand-file-name org-astro-debug-log-file)
                           (expand-file-name "ox-astro-debug.log" temporary-file-directory)))
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
          ;; Copy clipboard text to clipboard via pbcopy (opt-in)
          (when (and (boundp 'org-astro-copy-to-clipboard) org-astro-copy-to-clipboard)
            (let ((pbcopy (executable-find "pbcopy")))
              (when pbcopy
                (condition-case _
                    (with-temp-buffer
                      (insert clipboard-text)
                      (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                  (error nil)))))))
      ;; Append the actual log line
      (condition-case _
          (with-temp-buffer
            (insert line)
            (write-region (point-min) (point-max) debug-file t 'silent))
        (error nil)))))

(defun org-astro--dbg-update-output-file (info actual-output-file)
  "Update the debug file header with the actual output file path."
  (when (and (boundp 'org-astro-debug-images) org-astro-debug-images actual-output-file)
    (let* ((debug-file (if (boundp 'org-astro-debug-log-file)
                           (expand-file-name org-astro-debug-log-file)
                           (expand-file-name "ox-astro-debug.log" temporary-file-directory)))
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
          ;; Update clipboard (opt-in)
          (when (and (boundp 'org-astro-copy-to-clipboard) org-astro-copy-to-clipboard)
            (let ((pbcopy (executable-find "pbcopy")))
              (when pbcopy
                (condition-case _
                    (with-temp-buffer
                      (insert clipboard-text)
                      (call-process-region (point-min) (point-max) pbcopy nil nil nil))
                  (error nil))))))))))

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
              slug title astro-folder dest-folder filetags noexport)
          (goto-char (point-min))
          (when (re-search-forward "^#\\+FILETAGS:\\s-*\\(.+\\)$" nil t)
            (setq filetags (match-string 1))
            (when (and filetags
                       (string-match-p ":noexport:" (downcase filetags)))
              (setq noexport t)))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+NOEXPORT:\\s-*\\(.+\\)$" nil t)
            (let ((raw (match-string 1)))
              (when (org-astro--string-truthy-p raw)
                (setq noexport t))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+ROAM_EXCLUDE:\\s-*\\(.+\\)$" nil t)
            (let ((raw (match-string 1)))
              (when (org-astro--string-truthy-p raw)
                (setq noexport t))))
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
          (when (re-search-forward "^#\\+DESTINATION\\(?:[_-]FOLDER\\)?:\\s-*\\(.+\\)$" nil t)
            (setq dest-folder (string-trim (match-string 1))))
          (unless title
            (goto-char (point-min))
            (when (re-search-forward "^\\*+ \\(.+\\)$" nil t)
              (setq title (string-trim (match-string 1)))))
          (let* ((slug-final (if (and slug (not (string-blank-p slug)))
                                 slug
                                 (when (and title (not (string-blank-p title)))
                                   (org-astro--slugify title))))
                 ;; Resolve destination; if the file doesn't declare one,
                 ;; fall back to the first known posts folder so ID links
                 ;; still map to an outfile in the ID map.
                 (destination-info
                  (let ((resolved (org-astro--resolve-destination-config
                                   (or astro-folder dest-folder))))
                    (if (plist-get resolved :path)
                        resolved
                      (let* ((default-pair (car org-astro-known-posts-folders))
                             (fallback (and default-pair
                                            (org-astro--resolve-destination-config
                                             (car default-pair)))))
                        (or fallback resolved)))))
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
                  :noexport noexport
                  :destination-raw (plist-get destination-info :raw)
                  :source file))))
    (error
     (message "[ox-astro] Failed to inspect %s for ID metadata: %s"
              file (error-message-string err))
     nil)))

(defun org-astro--id-path-score (path)
  "Return a score for PATH to choose a canonical source.
Lower is better; penalize conflicted/copy variants, then use length as tiebreaker."
  (let* ((p (downcase path))
         (score (length path)))
    (when (string-match "conflicted copy" p)
      (cl-incf score 10000))
    (when (string-match "\\bcopy\\.org\\'" p)
      (cl-incf score 5000))
    score))

(defun org-astro--id-choose-winner (existing candidate)
  "Return the preferred entry between EXISTING and CANDIDATE."
  (let* ((path-existing (plist-get existing :source))
         (path-candidate (plist-get candidate :source))
         (score-existing (org-astro--id-path-score path-existing))
         (score-candidate (org-astro--id-path-score path-candidate)))
    (if (< score-candidate score-existing) candidate existing)))

(defun org-astro--id-map-store (map id meta &optional tracker)
  "Store ID mapping derived from META into MAP.
TRACKER, when non-nil, accumulates duplicate counts keyed by ID."
  (let* ((entry (list :source (plist-get meta :source)
                      :outfile (plist-get meta :outfile)
                      :filename (plist-get meta :filename)
                      :posts-folder (plist-get meta :posts-folder)
                      :relative-subdir (plist-get meta :relative-subdir)
                      :preserve (plist-get meta :preserve)
                      :slug (plist-get meta :slug)
                      :title (plist-get meta :title)
                      :destination-raw (plist-get meta :destination-raw)))
         (existing (gethash id map)))
    (cond
     ((not existing)
      (puthash id entry map)
      (when tracker
        (puthash id (list :id id :winner entry :count 0 :examples nil) tracker)))
     (t
      (let* ((winner (org-astro--id-choose-winner existing entry))
             (loser (if (eq winner existing) entry existing))
             (info (and tracker (gethash id tracker))))
        (puthash id winner map)
        (when tracker
          (let* ((count (1+ (or (plist-get info :count) 0)))
                 (examples (plist-get info :examples)))
            (puthash id (list :id id
                              :winner winner
                              :count count
                              :examples (cons (plist-get loser :source) examples))
                     tracker))))))))

(defun org-astro--build-id-map (source-dir)
  "Build and return a hash table mapping org-roam IDs in SOURCE-DIR.
Logs duplicate IDs once per ID with a summary of the kept/ignored sources.
Set environment variable OX_ASTRO_DUP_LOG_MODE=verbose to log every duplicate ID."
  (let* ((root (and source-dir (expand-file-name source-dir)))
         (map (make-hash-table :test #'equal))
         (duplicate-tracker (make-hash-table :test #'equal))
         (dup-log-mode (downcase (or (getenv "OX_ASTRO_DUP_LOG_MODE") "summary")))
         (dup-verbose (member dup-log-mode '("verbose" "full" "all" "1" "t"))))
    ;; Build map and track duplicates.
    (if (and root (file-directory-p root))
        (dolist (file (directory-files-recursively root "\\.org\\'"))
          (let ((meta (org-astro--collect-org-file-export-metadata file)))
            (when (and meta (not (plist-get meta :noexport)))
              (dolist (id (plist-get meta :id-list))
                (when (and id (not (string-blank-p id)))
                  (org-astro--id-map-store map id meta duplicate-tracker))))))
      (when root
        (message "[ox-astro] ID map skipped: source directory %s not found" root)))

    ;; Emit duplicate summary only once per batch (not for every file).
    (unless org-astro--duplicate-ids-logged
      (let ((dups '()))
        (maphash
         (lambda (_id info)
           (let ((count (plist-get info :count)))
             (when (> count 0)
               (push info dups))))
         duplicate-tracker)
        (setq dups (nreverse dups))
        (when dup-verbose
          (dolist (info dups)
            (message "[ox-astro] Duplicate org-roam ID %s: kept %s; skipped %d other%s%s"
                     (plist-get info :id)
                     (plist-get (plist-get info :winner) :source)
                     (plist-get info :count)
                     (if (= (plist-get info :count) 1) "" "s")
                     (if (plist-get info :examples)
                         (format " (e.g., %s)" (car (last (plist-get info :examples))))
                       ""))))
        (let ((total (length dups)))
          (when (> total 0)
            (let* ((sample (seq-take dups 8))
                   (examples
                    (mapconcat
                     (lambda (info)
                       (format "%s (kept %s; skipped %d)"
                               (plist-get info :id)
                               (plist-get (plist-get info :winner) :source)
                               (plist-get info :count)))
                     sample
                     "; ")))
              (message "[ox-astro] Duplicate org-roam IDs: %d total. Examples: %s. Set OX_ASTRO_DUP_LOG_MODE=verbose for full list."
                       total examples))))
        ;; Log ID map size on first build
        (message "[ox-astro] ID map built: %d IDs from root %s"
                 (hash-table-count map) root)
        ;; Mark as logged so we don't repeat for subsequent files
        (setq org-astro--duplicate-ids-logged t)))
    map))

(defun org-astro--add-file-to-id-map (map file)
  "Ensure FILE's IDs are present in MAP."
  (when (and map file (file-exists-p file))
    (let ((meta (org-astro--collect-org-file-export-metadata file)))
      (when (and meta (not (plist-get meta :noexport)))
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

(defun org-astro--compute-collection-id (entry)
  "Compute the Astro content collection ID from ENTRY metadata.
The collection ID is the path used in Astro routes, e.g., \"stories/my-story\".
This is derived from the relative-subdir and slug, or from the filename."
  (when entry
    (let* ((relative-subdir (plist-get entry :relative-subdir))
           (slug (plist-get entry :slug))
           (filename (plist-get entry :filename))
           ;; Get the base name without .mdx extension
           (base-name (cond
                       (slug slug)
                       (filename (file-name-sans-extension filename))
                       (t nil))))
      (when base-name
        ;; Combine relative-subdir with base-name
        ;; relative-subdir is like "stories/" or nil
        (if (and relative-subdir (not (string-blank-p relative-subdir)))
            ;; Remove trailing slash from subdir, combine with base-name
            (concat (string-trim-right relative-subdir "/") "/" base-name)
            base-name)))))

(defun org-astro--compute-collection-id-from-outfile (entry)
  "Compute collection ID by diffing OUTFILE against POSTS-FOLDER in ENTRY.
Falls back to stripping the .mdx extension from the outfile when slug data is
absent in the ENTRY."
  (let ((outfile (plist-get entry :outfile))
        (posts-folder (plist-get entry :posts-folder)))
    (when (and outfile posts-folder)
      (let* ((relative (file-relative-name outfile posts-folder))
             (no-ext (file-name-sans-extension relative)))
        (string-trim-left no-ext "./")))))

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
      (while (re-search-forward "^#\\+begin_src \\(user\\|prompt\\|quote\\|poetry\\|verse\\)" nil t)
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

(defun org-astro--date-string-p (s)
  "Return non-nil if string S looks like a numeric date (digits plus separators)."
  (when (and s (stringp s))
    (let* ((trimmed (string-trim s))
           (non-date-chars (replace-regexp-in-string "[0-9./_[:space:]-]" "" trimmed))
           (digits-only (replace-regexp-in-string "[^0-9]" "" trimmed)))
      (and (string-empty-p non-date-chars)
           (>= (length digits-only) 6)))))

(defun org-astro--slugify (s)
  "Convert string S to a slug.
Strips leading/trailing dashes that result from punctuation like question marks."
  (when (stringp s)
    (let* ((s (downcase s))
           (s (replace-regexp-in-string "[^a-z0-9]+" "-" (org-trim s) nil))
           ;; Strip leading and trailing dashes
           (s (replace-regexp-in-string "^-+" "" s))
           (s (replace-regexp-in-string "-+$" "" s)))
      s)))

;; Detect whether TEXT contains Markdown link syntax that should be preserved
;; as-is. We check for inline links [text](url) and reference-style links
;; [text][ref]. This is intentionally conservative to avoid false positives.
(defun org-astro--contains-markdown-link-p (text)
  "Return non-nil if TEXT contains Markdown link syntax.
Matches inline links like [label](https://example.com) and
reference-style links like [label][ref]."
  (when (and text (stringp text))
    (or (string-match-p "\\[[^]]+\\]([^)]+)" text)          ; inline [..](..)
        (string-match-p "\\[[^]]+\\]\\[[^]]+\\]" text))))   ; reference [..][..]

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

(defun org-astro--yaml-quote-string (s)
  "Return YAML-safe double-quoted string for S."
  (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" s)))

(defun org-astro--yaml-string-numeric-p (s)
  "Return non-nil if S looks like a purely numeric string."
  (and s (stringp s)
       (string-match-p "^[0-9]+$" (string-trim s))))

(defun org-astro--yaml-should-quote-string-p (s &optional key)
  "Decide if string S should be quoted for YAML.
KEY optionally provides field context."
  (or (null s)
      (memq key '(title slug excerpt))
      (org-astro--yaml-string-numeric-p s)
      (string-match-p "[\n\r]" s)
      (string-match-p "^[|>:\\[\\]{}&#*?,-]" s)
      (string-match-p "|" s)
      (and (not (eq key 'publishDate))
           (string-match-p ":" s))))

(defun org-astro--yaml-encode-scalar (key value)
  "Render VALUE for YAML key KEY as a scalar string or nil."
  (cond
   ((null value) nil)
   ((eq key 'publishDate)
    (when (and (stringp value) (not (string-empty-p value)))
      (string-trim value)))
   ((numberp value)
    (org-astro--yaml-quote-string (format "%s" value)))
   ((stringp value)
    (let ((trimmed (string-trim value)))
      (cond
       ((string-empty-p trimmed) nil)
       ((org-astro--yaml-should-quote-string-p trimmed key)
        (org-astro--yaml-quote-string trimmed))
       (t trimmed))))
   (t (org-astro--yaml-quote-string (format "%s" value)))))

(defun org-astro--yaml-format-list (items &optional indent key)
  "Format ITEMS as a YAML list. INDENT is applied to each row.
KEY is forwarded to scalar encoding for per-item quoting rules."
  (let* ((prefix (or indent ""))
         (clean-items (delq nil items))
         (rendered (delq nil (mapcar (lambda (item)
                                       (org-astro--yaml-encode-scalar key item))
                                     clean-items))))
    (mapconcat (lambda (item)
                 (format "%s- %s" prefix item))
               rendered
               "\n")))

(defun org-astro--format-tags-field (tags)
  "Return YAML snippet for TAGS, handling empty and single-tag cases."
  (cond
   ((null tags)
    "tags: []\n")
   ((= (length tags) 1)
    (format "tags: [%s]\n" (org-astro--yaml-encode-scalar 'tag (car tags))))
   (t
    (concat "tags:\n"
            (org-astro--yaml-format-list tags "  " 'tag)
            "\n"))))

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
          (let* ((key (car pair))
                 (val (cdr pair))
                 (key-name (symbol-name key)))
            (cond
             ((and (consp val) (eq (car val) :raw-yaml))
              (let ((raw (string-trim-right (cdr val))))
                (when (not (string-empty-p raw))
                  (setq yaml-str
                        (concat yaml-str
                                (format "%s:\n%s\n" key-name raw))))))
             ((eq key 'tags)
              (setq yaml-str (concat yaml-str (org-astro--format-tags-field val))))
             ((and (listp val) (null val))
              ;; Skip empty lists
              )
             ((not val)
              ;; Skip nil values
              )
             (t
              (setq yaml-str
                    (concat
                     yaml-str
                     (cond
                      ((listp val)
                       (let ((body (org-astro--yaml-format-list val "" key)))
                         (if (string-empty-p body)
                             ""
                           (format "%s:\n%s\n" key-name body))))
                      (t
                       (let ((rendered (org-astro--yaml-encode-scalar key val)))
                         (if rendered
                             (format "%s: %s\n" key-name rendered)
                           ""))))))))))
        (concat yaml-str "---\n"))))

(defun org-astro--get-title (tree info)
  "Return a title string from TREE/INFO, never nil."
  (let* ((input-file (or (plist-get info :input-file) (buffer-file-name)))
         (filename-base (and input-file (file-name-base input-file)))
         (headline (org-element-map tree 'headline 'identity nil 'first-match))
         (headline-title (when headline
                           (org-astro--safe-export (org-element-property :title headline) info)))
         (title-values (org-element-map tree 'keyword
                         (lambda (k)
                           (when (string-equal "TITLE" (org-element-property :key k))
                             (org-element-property :value k)))))
         (title-values (delq nil title-values))
         (non-date-title (cl-find-if (lambda (v) (not (org-astro--date-string-p v)))
                                     (reverse title-values)))
         (first-title (car title-values))
         (date-only (and (null non-date-title)
                         (or (and first-title (org-astro--date-string-p first-title))
                             (and filename-base (org-astro--date-string-p filename-base))))))
    (cond
     ;; Prefer any non-date #+TITLE (last one wins)
     (non-date-title non-date-title)
     ;; If title is just a date (or filename is), prefer the first headline.
     (date-only (or headline-title "Untitled Post"))
     ;; Explicit #+TITLE (all date-like)
     (first-title first-title)
     ;; Else first headline, safely exported
     (headline-title)
     ;; Fallback
     ("Untitled Post"))))

(defun org-astro--sanitize-excerpt (text)
  "Clean TEXT for safe YAML usage in excerpts."
  (when (and text (stringp text))
    (let* ((clean (string-trim text)))
      ;; Drop property drawer style lines like :ID: and other keys.
      (setq clean (replace-regexp-in-string "^:[A-Z_]+:\\s-*.*$" "" clean))
      ;; Remove roam preamble boilerplate (Links/Source lines).
      (setq clean (replace-regexp-in-string "^-[ \t]*\\*\\*\\(Links\\|Source\\):\\*\\*[ \t]*$" "" clean))
      ;; Collapse whitespace/newlines
      (setq clean (replace-regexp-in-string "[\n\r]+" " " clean))
      (setq clean (replace-regexp-in-string "[[:space:]]+" " " clean))
      ;; Cap length to keep front matter manageable
      (when (> (length clean) 300)
        (setq clean (concat (substring clean 0 297) "...")))
      (string-trim clean))))

(defun org-astro--get-excerpt (tree info)
  "Return an excerpt string from TREE/INFO, possibly empty but never nil.
Treats SUBHED/DESCRIPTION as fallbacks when EXCERPT is not present."
  (let* ((kw-excerpt (org-element-map tree 'keyword
                        (lambda (k)
                          (when (member (org-element-property :key k)
                                        '("ASTRO_EXCERPT" "EXCERPT" "SUBHED"))
                            k))
                        nil 'first-match))
         (kw-description (org-element-map tree 'keyword
                           (lambda (k)
                             (when (string= (org-element-property :key k) "DESCRIPTION")
                               k))
                           nil 'first-match))
         (first-paragraph (org-element-map tree 'paragraph 'identity nil 'first-match)))
    (or
     (org-astro--sanitize-excerpt
      (when kw-excerpt
        (let ((v (org-element-property :value kw-excerpt)))
          (string-trim (replace-regexp-in-string "[*_/]" "" v)))))
     (org-astro--sanitize-excerpt
      (when kw-description
        (let ((v (org-element-property :value kw-description)))
          (string-trim (replace-regexp-in-string "[*_/]" "" v)))))
     (org-astro--sanitize-excerpt
      (when first-paragraph
        (let* ((raw (org-astro--safe-export (org-element-contents first-paragraph) info))
               (clean (replace-regexp-in-string "[*_/]" "" raw))
               ;; Remove image tags like ![img](path) and <img...> tags
               (no-images (replace-regexp-in-string "!\\[.*?\\]([^)]*)" "" clean))
               (no-html-images (replace-regexp-in-string "<img[^>]*>" "" no-images))
               (one (replace-regexp-in-string "\n" " " no-html-images)))
          (if (string-match "^\\(.\\{1,300\\}?[.?!]\\)" one)
              (org-trim (match-string 1 one))
            (truncate-string-to-width (org-trim one) 300 nil nil "…")))))
     "")))


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
  (let* ((slug (or (plist-get info :slug)
                   (let ((title (or (plist-get info :title)
                                    (org-astro--get-title (plist-get info :parse-tree) info))))
                     (and title (org-astro--slugify title)))))
         (image-raw (or (plist-get info :astro-image)
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
    (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
      (org-astro--debug-log-direct "Hero image selection: explicit=%s fallback=%s final=%s" image fallback-image final-image))
    (setf (plist-get info :astro-hero-image)
          (org-astro--normalize-image-path final-image))
    (setf (plist-get info :astro-hero-body-suppressed) nil)
    (list final-image image-alt)))

(defun org-astro--format-connections-yaml (connections)
  "Return a YAML string (without trailing newline) for CONNECTIONS alist."
  (let ((lines nil))
    (dolist (pair connections)
      (let* ((key (symbol-name (car pair)))
             (values (cl-remove-if (lambda (item) (and (stringp item) (string-empty-p item)))
                                   (cdr pair))))
        (when values
          (push (format "  %s:" key) lines)
          (dolist (value values)
            (push (format "    - %s" value) lines)))))
    (when lines
      (string-join (nreverse lines) "\n"))))

(defun org-astro--get-org-roam-id (tree info)
  "Return first org-roam ID found in TREE or INFO."
  (or (org-element-map tree 'node-property
        (lambda (p)
          (when (string-equal (org-element-property :key p) "ID")
            (string-trim (or (org-element-property :value p) ""))))
        nil 'first-match)
      (org-element-map tree 'keyword
        (lambda (k)
          (when (string-equal (org-element-property :key k) "ID")
            (string-trim (or (org-element-property :value k) ""))))
        nil 'first-match)
      (plist-get info :ID)
      ;; Fallback: scan the source file for a top-level :ID: property
      (let ((file (or (plist-get info :input-file)
                      (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
        (when (and file (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file nil 0 (min 4096 (nth 7 (file-attributes file))))
            (goto-char (point-min))
            (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
              (string-trim (match-string 1))))))))

(defun org-astro--get-roam-aliases (tree info)
  "Return list of ROAM_ALIASES from TREE/INFO."
  (let* ((raw (or
               (org-element-map tree 'node-property
                 (lambda (p)
                   (when (member (org-element-property :key p)
                                 '("ROAM_ALIASES" "ROAM_ALIAS"))
                     (org-element-property :value p)))
                 nil 'first-match)
               (org-element-map tree 'keyword
                 (lambda (k)
                   (when (member (org-element-property :key k)
                                 '("ROAM_ALIASES" "ROAM_ALIAS"))
                     (org-element-property :value k)))
                 nil 'first-match)
               (plist-get info :roam-aliases)
               (plist-get info :roam-alias)
               ;; Fallback: scan the source file for a top-level ROAM_ALIASES property
               (let ((file (or (plist-get info :input-file)
                               (and (buffer-file-name) (expand-file-name (buffer-file-name))))))
                 (when (and file (file-readable-p file))
                   (with-temp-buffer
                     (insert-file-contents file nil 0 (min 4096 (nth 7 (file-attributes file))))
                     (goto-char (point-min))
                     (when (re-search-forward "^:ROAM_ALIASES:\\s-*\\(.+\\)$" nil t)
                       (match-string 1))))))))
    (org-astro--split-quoted-list raw)))

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
         (media (org-astro--parse-media tree info))
         (story-type (org-astro--get-story-type tree info))
         (hero-credit (org-astro--get-hero-credit tree info))
         (hero-caption (org-astro--get-hero-caption tree info))
         (hide-hero-image
          (let* ((raw (or (plist-get info :hide-hero-image)
                          (plist-get info :astro-hide-hero-image)))
                 (truthy (when raw (org-astro--string-truthy-p raw))))
            (when truthy "true")))
         (incomplete-token (org-astro--parse-incomplete tree info))
         (incomplete (cond
                      ((eq incomplete-token :true) "true")
                      ((eq incomplete-token :false) "false")
                      (t nil)))
         (org-roam-id (org-astro--get-org-roam-id tree info))
         (org-roam-aliases (org-astro--get-roam-aliases tree info))
         (org-path (or (plist-get info :input-file)
                       (and (buffer-file-name) (expand-file-name (buffer-file-name)))))
         (connections-data (org-astro--parse-connections tree info))
         (connections-yaml (org-astro--format-connections-yaml connections-data))
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
      (slug . ,slug)
      (author . ,author)
      (authorImage . ,author-image)
      (publishDate . ,publish-date)
      (excerpt . ,excerpt)
      (image . ,image)
      (imageAlt . ,image-alt)
      ,@(when hero-credit `((heroCredit . ,hero-credit)))
      ,@(when hero-caption `((heroCaption . ,hero-caption)))
      ,@(when hide-hero-image `((hideHeroImage . ,hide-hero-image)))
      ,@(when date-occurred `((dateOccurred . ,date-occurred)))
      (tags . ,tags)
      ,@(when place `((place . ,place)))
      ,@(when people `((people . ,people)))
      ,@(when emotions `((emotions . ,emotions)))
      ,@(when media `((media . ,media)))
      ,@(when places `((places . ,places)))
      ,@(when themes-list `((themes . ,themes-list)))
      ,@(when story-type `((storyType . ,story-type)))
      (categories . ,categories)
      ,@(when era `((era . ,era)))
      ,@(when incomplete `((incomplete . ,incomplete)))
      ,@(when org-roam-id `((orgRoamId . ,org-roam-id)))
      ,@(when org-roam-aliases `((aliases . ,org-roam-aliases)))
      ,@(when org-path `((orgPath . ,org-path)))
      ,@(when connections-yaml
          `((connections . (:raw-yaml . ,connections-yaml))))
      ,@(when visibility `((visibility . ,visibility)))
      ,@(when theme `((theme . ,theme)))
      ,@(when draft `((draft . ,draft))))))

(defun org-astro--validate-front-matter (data)
  "Validate and sanitize front-matter DATA alist.
Returns cleaned alist; emits warnings when coercions occur."
  (let ((warnings nil)
        (cleaned nil))
    (dolist (pair data)
      (let* ((key (car pair))
             (val (cdr pair)))
        (pcase key
          ((or 'title 'slug)
           (cond
            ((numberp val)
             (push (format "Converting numeric %s to string: %s" key val) warnings)
             (push (cons key (format "%s" val)) cleaned))
            ((stringp val)
             (push (cons key (string-trim val)) cleaned))
            (t
             (push (format "Coercing %s of type %s to string" key (type-of val)) warnings)
             (push (cons key (format "%s" val)) cleaned))))
          ('excerpt
           (let ((clean (org-astro--sanitize-excerpt val)))
             (when (and (stringp val) (not (string= (string-trim val) (or clean ""))))
               (push "Sanitized excerpt for YAML safety" warnings))
             (push (cons key (or clean "")) cleaned)))
          ('tags
           (let* ((as-list (cond
                            ((null val) nil)
                            ((listp val) val)
                            (t (list val))))
                  (clean-tags (delq nil (mapcar #'org-astro--clean-tag as-list)))
                  (dropped (- (length as-list) (length clean-tags))))
             (when (> dropped 0)
               (push (format "Filtered %d invalid tag(s)" dropped) warnings))
             (push (cons key clean-tags) cleaned)))
          (_
           (push pair cleaned)))))
    (when warnings
      (message "[ox-astro] Front matter warnings:\n%s"
               (string-join (nreverse warnings) "\n")))
    (nreverse cleaned)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcode Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-astro-underline (underline contents _info)
  "Transcode UNDERLINE without producing MDX spans.
- If the underline is just placeholder underscores (3+), emit the underscores.
- Otherwise, emit plain contents/value with no additional markup."
  (let* ((begin (org-element-property :begin underline))
         (end (org-element-property :end underline))
         (raw (and begin end
                   (buffer-substring-no-properties begin end)))
         (trimmed (and raw (string-trim-right raw)))
         (placeholder (and trimmed
                           (string-match "^_\\{3,\\}$" trimmed)
                           trimmed))
         (text (or contents (org-element-property :value underline))))
    (cond
     (placeholder placeholder)
     (text text)
     (t ""))))

(defun org-astro-link (link desc info)
  "Transcode a LINK object for Astro MDX, robust to partial INFO during preflight."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link)))
    (cond
     ;; org-roam ID links → resolve to absolute routes or relative Markdown links
     ((string= type "id")
      (let* ((target-id path)
             (entry (and org-astro--id-path-map
                         (gethash target-id org-astro--id-path-map)))
             (_ (unless entry
                  (message "[ox-astro DEBUG] ID lookup failed: %s (map has %d entries, map=%s)"
                           target-id
                           (if org-astro--id-path-map (hash-table-count org-astro--id-path-map) 0)
                           (if org-astro--id-path-map "exists" "NIL"))))
             (link-text (if (and desc (not (string-blank-p desc)))
                            desc
                            (or (and entry (plist-get entry :title))
                                target-id))))
        (if entry
            ;; Check if we should use absolute routes or relative MDX paths
            ;; Priority: dynamic var (from dest config) > global var
            (let ((effective-base-path (or org-astro--current-id-link-base-path
                                           (and (boundp 'org-astro-id-link-base-path)
                                                org-astro-id-link-base-path))))
              (if effective-base-path
                  ;; Absolute route mode: /base-path/collection-id
                  (let ((collection-id (or (org-astro--compute-collection-id entry)
                                           (org-astro--compute-collection-id-from-outfile entry))))
                    (if collection-id
                        (let* ((base (string-trim-right effective-base-path "/"))
                               (route (concat base "/" collection-id)))
                          (format "[%s](%s)" link-text route))
                        (org-astro--record-missing-id-link info target-id link-text)))
                  ;; Relative MDX path mode (original behavior)
                  (if org-astro--current-outfile
                      (let* ((target-outfile (or (plist-get entry :outfile)
                                                 (let* ((posts-folder (plist-get entry :posts-folder))
                                                        (filename (plist-get entry :filename))
                                                        (relative-dir (plist-get entry :relative-subdir)))
                                                   (when (and posts-folder filename)
                                                     (let ((base (if (and relative-dir (not (string-blank-p relative-dir)))
                                                                     (expand-file-name relative-dir posts-folder)
                                                                     posts-folder)))
                                                       (expand-file-name filename base)))))))
                        (if target-outfile
                            (let ((relative (org-astro--calculate-relative-mdx-path
                                             org-astro--current-outfile target-outfile)))
                              (if relative
                                  (format "[%s](%s)" link-text relative)
                                  (org-astro--record-missing-id-link info target-id link-text)))
                            (org-astro--record-missing-id-link info target-id link-text)))
                      (org-astro--record-missing-id-link info target-id link-text))))
            (org-astro--record-missing-id-link info target-id link-text))))
     ;; Local file image links → render Image component using imports
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (or (string-match-p "\\.\\(png\\|jpe?g\\|webp\\)$" path)
               (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" path)))
      (let* ((record (org-astro--lookup-render-record path info))
             (_ (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
                  (message "[ox-astro][img] LINK path=%s record=%s"
                           path (and record (plist-get record :var-name))))))
        (if record
            (let* ((var-name (plist-get record :var-name))
                   (default-alt (plist-get record :alt))
                   (alt (or desc default-alt "Image")))
              (if var-name
                  (org-astro--image-component-for-record record info alt path)
                  (plist-get record :jsx)))
            (let ((md (when (fboundp 'org-md-link)
                        (ignore-errors (org-md-link link desc info)))))
              (or md (or (org-element-property :raw-link link)
                         (concat (or type "file") ":" path)))))))
     ;; Audio file links → return empty string (import handled by image manifest)
     ((and (or (string= type "file")
               (and (null type) path (string-prefix-p "/" path)))
           path
           (string-match-p "\\.\\(mp3\\|wav\\|ogg\\|m4a\\|aac\\|flac\\)$" path))
      "")
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

     ;; Non-image file links (e.g., file+emacs) → regular Markdown link
     ((member type '("file+emacs" "file+sys"))
      (let* ((raw (or (org-element-property :raw-link link) path ""))
             (url (cond
                    ((string-prefix-p "file+emacs:" raw)
                     (concat "file://" (string-remove-prefix "file+emacs:" raw)))
                    ((string-prefix-p "file+sys:" raw)
                     (concat "file://" (string-remove-prefix "file+sys:" raw)))
                    ((string-prefix-p "file:" raw)
                     (concat "file://" (string-remove-prefix "file:" raw)))
                    (t raw)))
             (label (or desc (file-name-nondirectory path) url)))
        (format "[%s](%s)" label url)))

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

     ;; Bare YouTube URLs with no description → responsive embed
     ((and (null desc)
           (member type '("http" "https"))
           (org-astro--youtube-id-from-url
            (or (org-element-property :raw-link link)
                (and type path (concat type ":" path)))))
      (let* ((raw (or (org-element-property :raw-link link)
                      (and type path (concat type ":" path))))
             (video-id (org-astro--youtube-id-from-url raw)))
        (when video-id
          (setf (plist-get info :astro-uses-youtube-embed) t)
          (org-astro--youtube-embed video-id))))

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
For 'user', 'prompt', 'quote', 'poetry', 'verse', and 'pullquote' blocks,
preserve org-mode syntax literally - convert org headings to markdown equivalents."
  (if (not (org-export-read-attribute :attr_md src-block :textarea))
      (let* ((lang (org-element-property :language src-block))
             ;; Use :value to get raw content, preserving internal newlines.
             (code (org-element-property :value src-block)))
        ;; Handle pullquote blocks specially - wrap in div with blank lines
        (if (string-equal lang "pullquote")
            (let ((processed-code code))
              ;; Convert org-mode links to Markdown links inside pullquotes
              ;; [[path][description]] → [description](path)
              (setq processed-code (replace-regexp-in-string
                                    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                                    "[\\2](\\1)"
                                    processed-code))
              ;; [[path]] → [path](path)
              (setq processed-code (replace-regexp-in-string
                                    "\\[\\[\\([^]]+\\)\\]\\]"
                                    "[\\1](\\1)"
                                    processed-code))
              (concat "<div class=\"pullquote\">\n\n"
                      (org-trim processed-code)
                      "\n\n</div>\n"))
            (progn
              ;; For user/prompt/quote blocks, convert org-mode syntax to markdown
              (when (member lang '("user" "prompt" "quote" "poetry" "verse"))
                ;; Convert em dashes
                (when (string-match-p "---" code)
                  (setq code (replace-regexp-in-string "---" "—" code)))
                ;; Convert org headings to markdown headings
                (setq code (replace-regexp-in-string "^\\*\\*\\*\\* \\(.*\\)$" "#### \\1" code))
                (setq code (replace-regexp-in-string "^\\*\\*\\* \\(.*\\)$" "### \\1" code))
                (setq code (replace-regexp-in-string "^\\*\\* \\(.*\\)$" "## \\1" code))
                (setq code (replace-regexp-in-string "^\\* \\(.*\\)$" "# \\1" code))
                ;; Convert org-mode links to Markdown links
                ;; [[path][description]] → [description](path)
                (setq code (replace-regexp-in-string
                            "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                            "[\\2](\\1)"
                            code))
                ;; [[path]] → [path](path)
                (setq code (replace-regexp-in-string
                            "\\[\\[\\([^]]+\\)\\]\\]"
                            "[\\1](\\1)"
                            code)))
              ;; Trim trailing newlines/whitespace to prevent extra space at the end.
              (setq code (string-trim-right code))
              (format "```%s\n%s\n```" (or lang "") code))))
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
  (let* ((path (org-astro--extract-image-path-from-paragraph paragraph))
         (record (and path (org-astro--lookup-render-record path info))))
    (if record
        (org-astro--image-component-for-record record info nil path)
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
        (let* ((record (org-astro--lookup-render-record path info)))
          (when (and (boundp 'org-astro-debug-images) org-astro-debug-images)
            (message "[ox-astro][img] PARA path=%s record=%s"
                     path (and record (plist-get record :var-name))))
          (if record
              (org-astro--image-component-for-record record info nil path)
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
         (has-linkpeek nil)
         (processed-lines
          (mapcar
           (lambda (line)
             (let ((trimmed-line (org-trim line)))
               (if (org-astro--contains-markdown-link-p line)
                   ;; Preserve Markdown link formatting unchanged
                   line
                   (cond
                    ;; Raw image path (trust precomputed render map)
                    ((and trimmed-line
                          (or (string-match-p "^/.*\\.(png\\|jpe?g\\|webp)$" trimmed-line)
                              (string-match-p "assets/images/.*\\.(png\\|jpe?g\\|jpeg\\|webp)$" trimmed-line)))
                     (let ((record (org-astro--lookup-render-record trimmed-line info)))
                       (if record
                           (org-astro--image-component-for-record record info nil trimmed-line)
                           line)))
                    ;; Remote image URL
                    ((and trimmed-line
                          (string-match-p "^https?://.*\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp)\\(\\?.*\\)?$" trimmed-line))
                     (let ((record (org-astro--lookup-render-record trimmed-line info)))
                       (if record
                           (org-astro--image-component-for-record record info nil trimmed-line)
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

(defun org-astro--collect-images-from-tree (tree &optional info)
  "Collect all image paths from the parse TREE.
When INFO is provided, forward it to the manifest builder so callers
can share a single discovery pass."
  (let ((manifest (org-astro--build-image-manifest tree info)))
    (mapcar (lambda (entry) (plist-get entry :original-path))
            manifest)))

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
