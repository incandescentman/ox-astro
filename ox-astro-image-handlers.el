;;; ox-astro-image-handlers.el --- Image handling functions for ox-astro  -*- lexical-binding: t -*-

;;; Commentary:
;; Image handling functions extracted from ox-astro-helpers.el

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-astro-config)
(require 'cl-lib)
(require 'subr-x)

;; Declare functions from ox-astro-helpers
(declare-function org-astro--debug-log-direct "ox-astro-helpers")
(declare-function org-astro--dbg-log "ox-astro-helpers")
(declare-function org-astro--get-assets-folder "ox-astro-helpers")
(declare-function org-astro--sanitize-filename "ox-astro-helpers")
(declare-function org-astro--path-to-var-name "ox-astro-helpers")
(declare-function org-astro--filename-to-alt-text "ox-astro-helpers")
(declare-function org-astro--collect-raw-image-paths "ox-astro-helpers")
(declare-function org-astro--extract-image-path-from-paragraph "ox-astro-helpers")
(declare-function org-astro--hero-image-entry-p "ox-astro-helpers")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAGE HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst org-astro--image-extension-regexp
  "\\.\\(png\\|jpe?g\\|jpeg\\|gif\\|svg\\|webp\\)"
  "Regular expression that matches supported image file extensions.")

(defconst org-astro--audio-extension-regexp
  "\\.\\(mp3\\|wav\\|ogg\\|m4a\\|aac\\|flac\\)"
  "Regular expression that matches supported audio file extensions.")

(defconst org-astro--media-extension-regexp
  "\\.\\(png\\|jpe?g\\|jpeg\\|gif\\|svg\\|webp\\|mp3\\|wav\\|ogg\\|m4a\\|aac\\|flac\\)"
  "Regular expression that matches all supported media file extensions (images + audio).")

(defun org-astro--image-remote-p (path)
  "Return non-nil when PATH references a remote image."
  (and (stringp path)
       (or (string-prefix-p "http://" path)
           (string-prefix-p "https://" path)
           (string-prefix-p "//" path))))

(defun org-astro--gif-p (path)
  "Return non-nil if PATH is a GIF file.
GIFs are handled specially because Astro's Sharp image optimization
fails on animated GIFs with dimension limit errors."
  (and (stringp path)
       (string-suffix-p ".gif" (downcase path))))

(defun org-astro--image-query-stripped-path (path)
  "Return PATH without trailing query or fragment components."
  (if (and (stringp path)
           (string-match "\\`\\([^?#]+\\)[?#]" path))
      (match-string 1 path)
    path))

(defun org-astro--image-path-matches-p (path)
  "Return non-nil when PATH looks like an image reference."
  (when (stringp path)
    (let ((candidate (org-astro--image-query-stripped-path path))
          (case-fold-search t))
      (and candidate
           (string-match-p org-astro--image-extension-regexp candidate)))))

(defun org-astro--audio-path-matches-p (path)
  "Return non-nil when PATH looks like an audio file reference."
  (when (stringp path)
    (let ((candidate (org-astro--image-query-stripped-path path))
          (case-fold-search t))
      (and candidate
           (string-match-p org-astro--audio-extension-regexp candidate)))))

(defun org-astro--media-path-matches-p (path)
  "Return non-nil when PATH looks like any media file (image or audio)."
  (when (stringp path)
    (let ((candidate (org-astro--image-query-stripped-path path))
          (case-fold-search t))
      (and candidate
           (string-match-p org-astro--media-extension-regexp candidate)))))

(defun org-astro--image-manifest--ensure (manifest-table key source-file)
  "Ensure KEY exists in MANIFEST-TABLE with SOURCE-FILE metadata.
Return the manifest entry plist."
  (or (gethash key manifest-table)
      (let ((entry (list :original-path key
                         :source-file source-file
                         :remote (org-astro--image-remote-p key)
                         :occurrences nil)))
        (puthash key entry manifest-table)
        entry)))

(defun org-astro--image-manifest--line-number (position)
  "Return line number at buffer POSITION, or nil when POSITION is nil."
  (when position
    (save-excursion
      (goto-char position)
      (line-number-at-pos position))))

(defun org-astro--image-manifest--link-description (link)
  "Return the string description for LINK if present."
  (let ((begin (org-element-property :contents-begin link))
        (end (org-element-property :contents-end link)))
    (when (and begin end)
      (string-trim (buffer-substring-no-properties begin end)))))

(defun org-astro--image-manifest--normalize-occurrence (&rest kvs)
  "Normalize key/value pairs KVS into a plist without nil entries."
  (let (result)
    (while kvs
      (let ((key (pop kvs))
            (value (pop kvs)))
        (when value
          (setq result (plist-put result key value)))))
    result))

(defun org-astro--get-link-repeat-attr (link)
  "Check if LINK has :repeat t in its parent's #+ATTR_ORG.
Returns non-nil if the image should be repeated inline even when used as hero."
  (let* ((parent (org-element-property :parent link))
         (attr-org (and parent (org-element-property :attr_org parent))))
    (when attr-org
      ;; attr_org is a list of strings like (":width 300 :repeat t")
      (let ((attr-string (mapconcat #'identity attr-org " ")))
        (and (string-match-p ":repeat\\s-+t\\b" attr-string) t)))))

(defun org-astro--build-image-manifest (tree info)
  "Collect all image references for TREE/INFO into a manifest.
Each manifest entry is a plist with keys:
- :original-path — raw path as discovered
- :source-file  — absolute source filename when available
- :remote       — non-nil when the reference is remote
- :occurrences  — list of occurrence plists with metadata."
  (org-with-wide-buffer
   (let* ((source-file (or (plist-get info :input-file)
                           (and (buffer-file-name)
                                (expand-file-name (buffer-file-name)))))
          (manifest (make-hash-table :test #'equal))
          (order nil))
     (cl-labels
         ((register-entry (path origin &rest kvs)
                          ;; Use media matcher to include both images and audio files
                          (when (org-astro--media-path-matches-p path)
                            (let* ((entry (org-astro--image-manifest--ensure manifest path source-file))
                                   (occur (apply #'org-astro--image-manifest--normalize-occurrence
                                                 :origin origin kvs))
                                   (occurrences (plist-get entry :occurrences)))
                              ;; Mark audio files for special handling downstream
                              (when (org-astro--audio-path-matches-p path)
                                (setq entry (plist-put entry :audio t)))
                              (setq entry (plist-put entry :occurrences (append occurrences (list occur))))
                              (puthash path entry manifest)
                              (unless (member path order)
                                (setq order (append order (list path))))))))
       (let ((register-entry-fn #'register-entry))
         ;; Link-based references ([[file:...]] and remote URLs)
         (org-element-map tree 'link
           (lambda (link)
             (let* ((type (or (org-element-property :type link) "file"))
                    (path (or (org-element-property :path link)
                              (org-element-property :raw-link link)))
                    (repeat-attr (org-astro--get-link-repeat-attr link)))
               ;; Use media matcher to include both images and audio files
               (when (org-astro--media-path-matches-p path)
                 (funcall register-entry-fn path
                          (if (org-astro--image-remote-p path) 'remote-link 'link)
                          :link-type type
                          :description (org-astro--image-manifest--link-description link)
                          :begin (org-element-property :begin link)
                          :end (org-element-property :end link)
                          :line (org-astro--image-manifest--line-number (org-element-property :begin link))
                          :repeat repeat-attr)))))
         ;; Raw plain-text occurrences (absolute paths, assets paths, remote URLs)
         ;; Now uses media-extension-regexp to also find audio files
         (org-element-map tree 'plain-text
           (lambda (plain)
             (let* ((raw (org-element-property :value plain)))
               (when (stringp raw)
                 (let* ((begin (org-element-property :begin plain))
                        (line (org-astro--image-manifest--line-number begin))
                        (regex (concat "\\(/[^[:space:]]+" org-astro--media-extension-regexp "\\)\\b\\|"
                                       "\\(https?://[^[:space:]]+" org-astro--media-extension-regexp "\\(?:[?#][^[:space:]]*\\)?\\)\\|"
                                       "\\(//[^[:space:]]+" org-astro--media-extension-regexp "\\(?:[?#][^[:space:]]*\\)?\\)\\|"
                                       "\\(assets/images/[^[:space:]]+" org-astro--media-extension-regexp "\\)"))
                        (start 0))
                   (while (string-match regex raw start)
                     (let ((path (or (match-string 1 raw)
                                     (match-string 2 raw)
                                     (match-string 3 raw)
                                     (match-string 4 raw)))))
                       (funcall register-entry-fn path 'plain-text
                                :begin begin
                                :line line
                                :context raw)
                       (setq start (match-end 0))))))))
         ;; Paragraph repair (paths broken by subscript parsing)
         (org-element-map tree 'paragraph
           (lambda (paragraph)
             (let ((path (org-astro--extract-image-path-from-paragraph paragraph)))
               (when path
                 (funcall register-entry-fn path 'paragraph
                          :begin (org-element-property :begin paragraph)
                          :line (org-astro--image-manifest--line-number (org-element-property :begin paragraph))
                          :context (org-element-interpret-data paragraph))))))
         ;; Buffer-level raw scans (underscored paths prior to wrapping)
         (dolist (path (org-astro--collect-raw-image-paths))
           (funcall register-entry-fn path 'raw-buffer))
         ;; Convert hash table into ordered list of plists.
         (mapcar
          (lambda (key)
            (gethash key manifest))
          order))))))

(defun org-astro--rewrite-org-image-path (original-path new-path)
  "Rewrite ORIGINAL-PATH to NEW-PATH in the source Org buffer.
Returns non-nil when the buffer was modified."
  (when (and original-path new-path
             (not (string-equal original-path new-path)))
    (org-astro--update-source-buffer-image-path original-path new-path)))

(defun org-astro--image-entry-alt (entry)
  "Derive a human-readable alt string for ENTRY."
  (or (cl-loop for occurrence in (plist-get entry :occurrences)
               for desc = (plist-get occurrence :description)
               when (and desc (not (string-blank-p desc)))
               return desc)
      (let ((path (or (plist-get entry :original-path)
                      (plist-get entry :path)
                      (plist-get entry :astro-path))))
        (when path
          (org-astro--filename-to-alt-text path)))
      "Image"))

(defun org-astro--materialize-image (entry posts-folder sub-dir)
  "Copy or download the image for ENTRY into Astro assets.
Returns a plist describing the outcome or nil on failure.
GIFs are copied to public/ folder instead of assets/ to bypass
Astro's Sharp image optimization which fails on animated GIFs."
  (let* ((original (plist-get entry :original-path))
         (is-gif (org-astro--gif-p original))
         (assets-folder (and posts-folder
                             (org-astro--get-assets-folder posts-folder sub-dir)))
         (public-folder (and posts-folder is-gif
                             (org-astro--get-public-folder posts-folder sub-dir))))
    (when (and original (not (string-empty-p original)) (or assets-folder public-folder))
      (let* ((expanded-original (when (and original (not (org-astro--image-remote-p original)))
                                  (expand-file-name (substring-no-properties original))))
             (assets-root (and assets-folder (file-name-as-directory (expand-file-name assets-folder)))))
        (cond
         ;; GIF already in public/images/ folder - extract web path, skip copy
         ((and is-gif expanded-original
               (file-exists-p expanded-original)
               (string-match "/public/images/\\(.+\\)$" expanded-original))
          (let* ((relative-path (match-string 1 expanded-original))
                 (web-path (concat "/images/" relative-path)))
            (message "[ox-astro][img] GIF already in public: %s" web-path)
            (list :public-path web-path
                  :target-path expanded-original
                  :rewrite-path nil
                  :is-public t
                  :original-path original  ;; Store original for render map lookup
                  :status 'existing-public-gif)))

         ;; GIF files go to public/ folder for static serving (no optimization)
         ((and is-gif expanded-original (file-exists-p expanded-original))
          (let* ((source-path expanded-original)
                 (original-filename (file-name-nondirectory source-path))
                 (clean-filename (org-astro--sanitize-filename original-filename))
                 (target-path (expand-file-name clean-filename public-folder))
                 (web-path (concat "/images/" sub-dir clean-filename)))
            (make-directory public-folder t)
            (condition-case err
                (copy-file source-path target-path t)
              (error
               (message "[ox-astro][img] Failed to copy GIF %s → %s (%s)" source-path target-path err)
               (setq target-path nil)))
            (when target-path
              (message "[ox-astro][img] GIF copied to public: %s" web-path)
              (list :public-path web-path
                    :target-path target-path
                    :rewrite-path nil  ;; Don't rewrite org file - keep original path
                    :is-public t
                    :status 'public-gif))))

         ;; Already inside the assets directory – reuse as-is.
         ((and expanded-original
               (string-prefix-p assets-root (file-name-directory (expand-file-name expanded-original))))
          (let* ((filename (file-name-nondirectory expanded-original))
                 (astro-path (concat "~/assets/images/" sub-dir filename)))
            (list :astro-path astro-path
                  :target-path expanded-original
                  :rewrite-path nil
                  :status 'existing)))

         ;; Remote URL – download to assets directory (or public for GIFs).
         ((org-astro--image-remote-p original)
          (let* ((full-url (if (string-prefix-p "//" original)
                               (concat "https:" original)
                             original))
                 (downloaded (org-astro--download-remote-image full-url posts-folder sub-dir)))
            (when downloaded
              (let* ((filename (file-name-nondirectory downloaded))
                     (astro-path (concat "~/assets/images/" sub-dir filename)))
                ;; If the downloaded file is a GIF, move it to public
                (if (org-astro--gif-p filename)
                    (let* ((clean-filename (org-astro--sanitize-filename filename))
                           (public-target (expand-file-name clean-filename public-folder))
                           (web-path (concat "/images/" sub-dir clean-filename)))
                      (make-directory public-folder t)
                      (rename-file downloaded public-target t)
                      (list :public-path web-path
                            :target-path public-target
                            :rewrite-path nil  ;; Don't rewrite org file for GIFs
                            :is-public t
                            :status 'public-gif))
                  (list :astro-path astro-path
                        :target-path downloaded
                        :rewrite-path downloaded
                        :status 'remote))))))

         ;; Local file – copy into assets directory with sanitized filename.
         (t
         (let* ((source-path expanded-original)
                 (original-filename (and source-path (file-name-nondirectory source-path)))
                 (clean-filename (and original-filename (org-astro--sanitize-filename original-filename)))
                 (target-path (and clean-filename (expand-file-name clean-filename assets-folder))))
            (cond
             ((not source-path)
              (message "[ox-astro][img] Skipping image without resolvable path: %s" original)
              nil)
             ((not (file-exists-p source-path))
              (message "[ox-astro][img] Source image missing: %s" source-path)
              nil)
             ;; Source and target resolve to the same file (e.g., symlinked Dropbox paths)
             ((and target-path (file-exists-p target-path) (file-equal-p source-path target-path))
              (let ((astro-path (concat "~/assets/images/" sub-dir clean-filename)))
                (list :astro-path astro-path
                      :target-path target-path
                      :rewrite-path nil
                      :status 'existing)))
             (t
              (make-directory assets-folder t)
              (condition-case err
                  (copy-file source-path target-path t)
                (error
                 (message "[ox-astro][img] Failed to copy %s → %s (%s)" source-path target-path err)
                 (setq target-path nil)))
              (when target-path
                (let ((astro-path (concat "~/assets/images/" sub-dir clean-filename)))
                  (list :astro-path astro-path
                        :target-path target-path
                        :rewrite-path target-path
                        :status 'copied))))))))))))

(defun org-astro--build-render-map (processed &optional hero-path)
  "Return render metadata for PROCESSED media entries (images and audio).
The result is a plist with keys:
- :map — hash table mapping lookup keys to render records
- :imports — list of import lines to include in the MDX prolog.
Public images (GIFs) get markdown syntax instead of <Image> components."
  (let ((render-map (make-hash-table :test #'equal))
        (imports nil)
        (include-image-component nil))
    (dolist (entry processed)
      (let* ((astro-path (plist-get entry :astro-path))
             (public-path (plist-get entry :public-path))
             (is-public (plist-get entry :is-public))
             (var-name (plist-get entry :var-name))
             (path (plist-get entry :path))
             (original (plist-get entry :original-path))
             (is-audio (org-astro--audio-path-matches-p (or astro-path public-path path original))))
        (cond
         ;; Public images (GIFs) - use markdown syntax, no import needed
         (is-public
          (let* ((alt (org-astro--image-entry-alt entry))
                 (escaped-alt (when alt (replace-regexp-in-string "\"" "\\\\\"" alt)))
                 (jsx (format "![%s](%s)" escaped-alt public-path))
                 (repeat-inline (plist-get entry :repeat))
                 (record (list :entry entry
                               :public-path public-path
                               :alt alt
                               :jsx jsx
                               :is-public t
                               :repeat repeat-inline))
                 (is-hero (and hero-path (org-astro--hero-image-entry-p entry hero-path))))
            (when is-hero
              (setf (plist-get entry :hero) t)
              (setf (plist-get record :hero) t))
            ;; No import for public images
            (dolist (key (list path
                               original
                               public-path
                               (and public-path
                                    (org-astro--sanitize-filename
                                     (file-name-sans-extension
                                      (file-name-nondirectory public-path))))))
              (when (and key (stringp key) (not (string-empty-p key)))
                (puthash key record render-map)))))

         ;; Regular assets images - use <Image> component with import
         ((and astro-path var-name)
          (let* ((alt (unless is-audio (org-astro--image-entry-alt entry)))
                 (escaped-alt (when alt (replace-regexp-in-string "\"" "\\\\\"" alt)))
                 (layout-prop (when (and (not is-audio)
                                         (boundp 'org-astro-image-default-layout)
                                         org-astro-image-default-layout
                                         (not (string= org-astro-image-default-layout "none")))
                                (format " layout=\"%s\"" org-astro-image-default-layout)))
                 ;; For audio files, don't generate JSX - just import for manual use
                 (jsx (unless is-audio
                        (format "<Image src={%s} alt=\"%s\"%s />" var-name escaped-alt (or layout-prop ""))))
                 (import-line (format "import %s from '%s';" var-name astro-path))
                 (repeat-inline (plist-get entry :repeat))
                 (record (list :entry entry
                               :var-name var-name
                               :astro-path astro-path
                               :alt alt
                               :jsx jsx
                               :audio is-audio
                               :repeat repeat-inline))
                 (is-hero (and (not is-audio) hero-path (org-astro--hero-image-entry-p entry hero-path))))
            (when is-hero
              (setf (plist-get entry :hero) t)
              (setf (plist-get record :hero) t))
            (cl-pushnew import-line imports :test #'equal)
            ;; Only include Image component for actual images, not audio
            (unless is-audio
              (setq include-image-component t))
            (dolist (key (list path
                               original
                               astro-path
                               (and astro-path
                                    (org-astro--sanitize-filename
                                     (file-name-sans-extension
                                      (file-name-nondirectory astro-path))))))
              (when (and key (stringp key) (not (string-empty-p key)))
                (puthash key record render-map))))))))
    (when include-image-component
      (cl-pushnew "import { Image } from 'astro:assets';" imports :test #'equal))
    (list :map render-map
          :imports (nreverse (cl-remove-duplicates imports :test #'equal)))))

(defun org-astro--process-image-manifest (manifest posts-folder sub-dir &optional opts)
  "Process MANIFEST entries for POSTS-FOLDER/SUB-DIR.
Returns a plist with keys:
- :entries — processed image entries suitable for downstream consumers
- :buffer-modified — non-nil when the source buffer was rewritten."
  (let* ((update-buffer (if (plist-member opts :update-buffer)
                            (plist-get opts :update-buffer)
                          t))
         (processed nil)
         (buffer-modified nil))
    (dolist (entry manifest)
      (let ((result (org-astro--materialize-image entry posts-folder sub-dir)))
        (when result
          (let* ((original (plist-get entry :original-path))
                 (rewrite (plist-get result :rewrite-path))
                 (final-path (or rewrite original))
                 (astro-path (plist-get result :astro-path))
                 (target-path (plist-get result :target-path))
                 (occurrences (plist-get entry :occurrences))
                 (var-name (when astro-path (org-astro--path-to-var-name astro-path)))
                 ;; For remote URLs parsed by org-mode (type + path), reconstruct the full URL
                 ;; Org parses [[https://example.com/img.jpg]] as type="https" path="//example.com/img.jpg"
                 (search-path (if (and (plist-get entry :remote)
                                       (cl-some (lambda (occ) (plist-get occ :link-type)) occurrences))
                                  (let ((link-type (cl-loop for occ in occurrences
                                                            for lt = (plist-get occ :link-type)
                                                            when lt return lt)))
                                    (if (and link-type (string-prefix-p "//" original))
                                        (concat link-type ":" original)
                                        original))
                                  original)))
            (org-astro--dbg-log nil "[PROCESS] original=%s search-path=%s rewrite=%s update-buffer=%s"
                                original search-path rewrite update-buffer)
            (when (and update-buffer rewrite
                       (org-astro--rewrite-org-image-path search-path rewrite))
              (setq buffer-modified t))
            ;; Check if any occurrence has :repeat t
            (let ((has-repeat (cl-some (lambda (occ) (plist-get occ :repeat)) occurrences))
                  (is-public (plist-get result :is-public))
                  (public-path (plist-get result :public-path)))
              (push (list :path final-path
                          :original-path original
                          :astro-path astro-path
                          :public-path public-path
                          :is-public is-public
                          :target-path target-path
                          :var-name var-name
                          :occurrences occurrences
                          :repeat has-repeat
                          :source-file (plist-get entry :source-file))
                    processed))))))
    (list :entries (nreverse processed)
          :buffer-modified buffer-modified)))

(defun org-astro--update-image-path-in-buffer (old-path new-path)
  "Replace OLD-PATH with NEW-PATH in the current buffer.
  This updates:
  - Org links [[file:OLD]][DESC] → [[file:NEW]][DESC]
  - Bare org links [[OLD]][DESC] → [[NEW]][DESC]
  - Raw lines containing only the path (ignoring surrounding whitespace)."
  (save-excursion
    (goto-char (point-min))
    (let ((changes-made nil))
      ;; 1) Update [[file:OLD]] and [[file:OLD][DESC]]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\(\\[[^]]*\\]\\)?\\]" nil t)
        (let ((match-beg (match-beginning 1))
              (match-end (match-end 1))
              (captured (match-string 1)))
          (when (string-equal captured old-path)
            (goto-char match-beg)
            (delete-region match-beg match-end)
            (insert new-path)
            (setq changes-made t))))

      ;; 1b) Update remote links [[https://OLD]] / [[http://OLD]] / [[//OLD]]
      (goto-char (point-min))
      (let ((escaped (regexp-quote old-path)))
        (while (re-search-forward (format "\\[\\[%s\\]\\(\\[[^]]*\\]\\)?\\]" escaped) nil t)
          (let* ((desc (match-string 1))
                 ;; Keep the original description (desc includes surrounding brackets).
                 (replacement (if desc
                                  (format "[[%s]%s]" new-path desc)
                                (format "[[%s]]" new-path))))
            (replace-match replacement t t)
            (setq changes-made t))))

      ;; 2) Update bare [[OLD]] and [[OLD][DESC]] (Org treats these as file links too)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(/[^]]+\\)\\]\\(\\[[^]]*\\]\\)?\\]" nil t)
        (let ((match-beg (match-beginning 1))
              (match-end (match-end 1))
              (captured (match-string 1)))
          (when (string-equal captured old-path)
            (goto-char match-beg)
            (delete-region match-beg match-end)
            (insert new-path)
            (setq changes-made t))))

      ;; 3) Update raw lines containing only the path (allowing whitespace)
      (goto-char (point-min))
      (let ((search-pattern (format "^[\t ]*%s[\t ]*$" (regexp-quote old-path))))
        (while (re-search-forward search-pattern nil t)
          (replace-match new-path t t)
          (setq changes-made t)))

      changes-made)))

(defun org-astro--update-image-path-in-file (file old-path new-path)
  "Open FILE, replace OLD-PATH with NEW-PATH using the same rules as buffer updater, and save.
               Returns non-nil if any changes were made."
  (when (and (stringp file) (file-exists-p file)
             (stringp old-path) (stringp new-path))
    (with-current-buffer (find-file-noselect file)
      (let ((changed (org-astro--update-image-path-in-buffer old-path new-path)))
        (when changed (save-buffer))
        changed))))

(defun org-astro--update-source-buffer-image-path (old-path new-path)
  "Update image path in the original source buffer, not the export copy.
               This function finds the source buffer and modifies it directly."
  ;; Strategy 1: Check if current buffer has a file name and is writable
  (let ((source-buffer nil))
    (cond
     ;; Current buffer is the source file (but NOT if it's a temp export buffer)
     ((and (buffer-file-name)
           (not buffer-read-only)
           (not (string-match-p "^ \\*temp\\*" (buffer-name))))
      (setq source-buffer (current-buffer)))

     ;; Try to find source buffer by examining all buffers
     (t
      (catch 'found-buffer
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (let ((buf-file (buffer-file-name)))
              (when (and buf-file
                         (not buffer-read-only)
                         (string-match-p "\\.org$" buf-file)
                         ;; Check if this buffer contains the image path (raw or bracketed)
                         (save-excursion
                           (goto-char (point-min))
                           (or (search-forward old-path nil t)
                               (search-forward (format "[[%s]]" old-path) nil t))))
                (setq source-buffer buf)
                (throw 'found-buffer nil))))))))

    ;; Now use the source-buffer within the same let binding
    (when source-buffer
      (with-current-buffer source-buffer
        (let ((changes-made (org-astro--update-image-path-in-buffer old-path new-path)))
          (when changes-made
            (save-buffer))
          changes-made)))))

(defun org-astro--get-assets-folder (posts-folder sub-dir)
  "Get the assets folder based on POSTS-FOLDER and SUB-DIR."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; Go up from content/blog to src
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir))))))
      (expand-file-name (concat "assets/images/" sub-dir) src-dir))))

(defun org-astro--get-public-folder (posts-folder sub-dir)
  "Get the public folder for static assets based on POSTS-FOLDER and SUB-DIR.
GIFs and other files that should bypass Astro's image optimization go here."
  (when posts-folder
    (let* ((posts-dir (file-name-as-directory (expand-file-name posts-folder)))
           ;; Go up from content/blog to app root (past src)
           (src-dir (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (directory-file-name posts-dir)))))
           (app-root (file-name-directory (directory-file-name src-dir))))
      (expand-file-name (concat "public/images/" sub-dir) app-root))))

;; ------------------------------------------------------------------
;; Image path suggestions block (for manual replacement workflow)
;; ------------------------------------------------------------------

;; Preprocessing: wrap raw absolute image paths in org link brackets [[...]]
(defun org-astro--wrap-raw-image-path-lines-in-region (beg end)
  "Within BEG..END, wrap raw absolute image path lines with Org link brackets.
               Only wraps lines that are just an absolute image path (png/jpg/jpeg/webp),
               and are not already an Org link. Returns number of lines changed."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line-content (buffer-substring-no-properties line-start line-end)))
            (cond
             ;; Skip lines that already have brackets
             ((string-match "\\[\\[.*\\]\\]" line-content)
              (forward-line 1))
             ;; Process lines that are only raw image paths
             ((string-match "^\\s-*\\(/[^[:space:]]*\\.\\(?:png\\|jpe?g\\|webp\\)\\)\\s-*$" line-content)
              (let ((path (match-string 1 line-content)))
                (delete-region line-start line-end)
                (insert (format "[[%s]]" path))
                (setq count (1+ count))
                (forward-line 1)))
             ;; Skip other lines
             (t (forward-line 1)))))))
    count))

(defun org-astro--persist-wrap-raw-image-lines (file)
  "Open FILE, wrap raw absolute image path lines with [[...]], save, and return count of lines changed."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (let ((cnt (org-astro--wrap-raw-image-path-lines-in-region (point-min) (point-max))))
          (when (> cnt 0)
            (save-buffer))
          cnt)))))


(defun org-astro--generate-image-paths-comment-block (items)
  "Generate a comment block with suggested image path replacements.
               ITEMS is a list of plists containing :path (old), :target-path (abs new), :astro-path (alias)."
  (let ((lines (list "# BEGIN ASTRO IMAGE PATH SUGGESTIONS"
                     "# Suggested replacements (old → new). Use alias for MDX."
                     (format "# Generated: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
                     "#")))
    (dolist (it items)
      (let* ((old (plist-get it :path))
             (new (or (plist-get it :target-path) ""))
             (alias (or (plist-get it :astro-path) "")))
        (push (format "# - old: %s" old) lines)
        (push (format "#   new: %s" new) lines)
        (push (format "#   alias: %s" alias) lines)
        (push "#" lines)))
    (setq lines (nreverse lines))
    (mapconcat #'identity (append lines (list "# END ASTRO IMAGE PATH SUGGESTIONS")) "\n")))

(defun org-astro--insert-or-replace-suggestions-block (block-text)
  "Insert or replace the image suggestions comment BLOCK-TEXT near the top."
  (save-excursion
    (goto-char (point-min))
    (let* ((limit (save-excursion (or (re-search-forward "^\\*" nil t) (point-max))))
           (begin-marker (progn
                           (save-restriction
                             (narrow-to-region (point-min) limit)
                             (goto-char (point-min))
                             (re-search-forward "^# BEGIN ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
           (end-marker (when begin-marker
                         (save-excursion
                           (re-search-forward "^# END ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
           (insert-point
            (unless begin-marker
              ;; Compute where to insert: after roam preamble and keyword/comment block
              (goto-char (point-min))
              ;; Skip properties block
              (when (looking-at-p "^:PROPERTIES:")
                (forward-line 1)
                (while (and (< (point) limit)
                            (not (looking-at-p "^:END:")))
                  (forward-line 1))
                (when (looking-at-p "^:END:") (forward-line 1)))
              ;; Skip roam preamble lines
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
                    ;; Else skip existing keywords/comments/blank
                    (while (and (< (point) limit)
                                (or (looking-at-p "^#\\+") (looking-at-p "^#\\s-") (looking-at-p "^\\s-*$")))
                      (forward-line 1))))
              (point))))
      (if begin-marker
          (progn
            (goto-char begin-marker)
            (beginning-of-line)
            (let ((start (point)))
              (when end-marker
                (goto-char end-marker)
                (end-of-line)
                (forward-char 1))
              (delete-region start (point)))
            (insert block-text "\n"))
          ;; Insert new block
          (goto-char insert-point)
          (unless (or (bobp) (looking-at-p "^\\s-*$")) (insert "\n"))
          (insert block-text "\n")))))

(defun org-astro--upsert-image-paths-comment-in-current-buffer (items)
  "Create or update the suggestions comment block in the current buffer."
  (let ((block (org-astro--generate-image-paths-comment-block items)))
    (org-astro--insert-or-replace-suggestions-block block)))

(defun org-astro--upsert-image-paths-comment (items)
  "Find the source Org buffer and upsert the suggestions comment block."
  (let ((source-buffer nil))
    (cond
     ((and (buffer-file-name) (not buffer-read-only))
      (setq source-buffer (current-buffer)))
     (t
      (catch 'found-buffer
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (let ((bf (buffer-file-name)))
              (when (and bf (not buffer-read-only)
                         (string-match-p "\\.org$" bf))
                (setq source-buffer buf)
                (throw 'found-buffer nil))))))))
    (when source-buffer
      (with-current-buffer source-buffer
        (org-astro--upsert-image-paths-comment-in-current-buffer items)
        (save-buffer)))))

(defun org-astro--upsert-image-paths-comment-into-file (file items)
  "Open FILE and upsert the image suggestions comment block, then save."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-astro--upsert-image-paths-comment-in-current-buffer items)
      (save-buffer))))

;; ------------------------------------------------------------------
;; Apply suggested image path replacements (manual, explicit action)
;; ------------------------------------------------------------------

(defun org-astro--parse-image-suggestions ()
  "Parse the suggestions block and return a list of plist items:
               Each item has :old, :new, and :alias keys.
               Returns nil if no suggestions block is found."
  (save-excursion
    (goto-char (point-min))
    (let* ((begin (re-search-forward "^# BEGIN ASTRO IMAGE PATH SUGGESTIONS$" nil t))
           (end (when begin (re-search-forward "^# END ASTRO IMAGE PATH SUGGESTIONS$" nil t))))
      (when (and begin end)
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (let (items current-old current-new current-alias)
            (while (re-search-forward "^# - old: \(.*\)$" nil t)
              (setq current-old (org-trim (match-string 1)))
              (when (re-search-forward "^#   new: \(.*\)$" nil t)
                (setq current-new (org-trim (match-string 1))))
              (when (re-search-forward "^#   alias: \(.*\)$" nil t)
                (setq current-alias (org-trim (match-string 1))))
              (when (and current-old (or current-new current-alias))
                (push (list :old current-old :new current-new :alias current-alias) items))
              (setq current-old nil current-new nil current-alias nil))
            (nreverse items)))))))

(defun org-astro-apply-image-path-replacements (&optional use-alias)
  "Apply suggested image path replacements in the current buffer.
               When USE-ALIAS is non-nil (interactive prefix), use :alias paths; otherwise use :new absolute paths."
  (interactive "P")
  (let* ((items (org-astro--parse-image-suggestions))
         (count 0)
         (mode (if use-alias :alias :new)))
    (if (not items)
        (message "No image suggestions block found.")
        (save-excursion
          (save-restriction
            (widen)
            (dolist (it items)
              (let* ((old (plist-get it :old))
                     (rep (plist-get it mode)))
                (when (and old rep (not (string-empty-p rep)))
                  (setq count (+ count (if (org-astro--update-image-path-in-buffer old rep) 1 0))))))
            (when (> count 0)
              (save-buffer)))
          (message "Applied replacements for %d image(s) using %s paths." count (if use-alias "alias" "absolute")))))
  )

(defun org-astro-apply-image-path-replacements-in-file (file &optional use-alias)
  "Open FILE, apply replacements from its suggestions block, and save.
When USE-ALIAS is non-nil, use :alias paths; otherwise use :new."
  (interactive "fOrg file: \nP")
  (with-current-buffer (find-file-noselect file)
    (org-astro-apply-image-path-replacements use-alias)))


(defun org-astro--sanitize-filename (filename)
  "Sanitize FILENAME by replacing underscores with hyphens and removing problematic characters."
  (let ((clean-name filename))
    ;; Replace underscores with hyphens to avoid subscript parsing issues
    (setq clean-name (replace-regexp-in-string "_" "-" clean-name))
    ;; Remove or replace other potentially problematic characters
    (setq clean-name (replace-regexp-in-string "[^a-zA-Z0-9.-]" "-" clean-name))
    ;; Remove multiple consecutive hyphens
    (setq clean-name (replace-regexp-in-string "--+" "-" clean-name))
    clean-name))

(defconst org-astro--remote-image-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0 Safari/537.36"
  "User-Agent string for remote image downloads.")

(defun org-astro--read-file-bytes (path max-bytes)
  "Return up to MAX-BYTES from PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 max-bytes)
    (buffer-string)))

(defun org-astro--binary-prefix-p (prefix data)
  "Return non-nil when DATA starts with PREFIX."
  (and (stringp data)
       (>= (length data) (length prefix))
       (string= (substring data 0 (length prefix)) prefix)))

(defun org-astro--data-looks-like-html-p (data)
  "Return non-nil when DATA looks like HTML or an error page."
  (let ((case-fold-search t))
    (or (string-match-p "<!doctype\\s-+html" data)
        (string-match-p "<html\\b" data)
        (string-match-p "<head\\b" data)
        (string-match-p "<body\\b" data)
        (string-match-p "access denied" data)
        (string-match-p "forbidden" data)
        (string-match-p "unauthorized" data)
        (string-match-p "request blocked" data))))

(defun org-astro--detect-image-type-from-bytes (data)
  "Return an image type symbol for DATA when recognized, nil otherwise."
  (or (condition-case nil
          (image-type-from-data data)
        (error nil))
      (cond
       ((org-astro--binary-prefix-p (string #x89 ?P ?N ?G ?\r ?\n #x1A ?\n) data) 'png)
       ((or (org-astro--binary-prefix-p "GIF87a" data)
            (org-astro--binary-prefix-p "GIF89a" data)) 'gif)
       ((org-astro--binary-prefix-p (string #xFF #xD8 #xFF) data) 'jpeg)
       ((and (>= (length data) 12)
             (string= (substring data 0 4) "RIFF")
             (string= (substring data 8 12) "WEBP")) 'webp)
       ((let ((case-fold-search t))
          (string-match-p "<svg\\b" data)) 'svg)
       (t nil))))

(defun org-astro--image-type->extension (type)
  "Return a canonical file extension for TYPE."
  (pcase type
    ('jpeg "jpg")
    ('jpg "jpg")
    ('png "png")
    ('gif "gif")
    ('webp "webp")
    ('svg "svg")
    (_ nil)))

(defun org-astro--extension-matches-image-type-p (path type)
  "Return non-nil when PATH's extension matches TYPE."
  (let ((ext (downcase (file-name-extension path ""))))
    (pcase type
      ((or 'jpeg 'jpg) (member ext '("jpg" "jpeg")))
      ('png (string= ext "png"))
      ('gif (string= ext "gif"))
      ('webp (string= ext "webp"))
      ('svg (string= ext "svg"))
      (_ t))))

(defun org-astro--strip-image-extension-chain (path)
  "Remove trailing image extensions from PATH (e.g., foo.png.webp -> foo)."
  (let ((case-fold-search t)
        (base path)
        (changed t))
    (while changed
      (setq changed nil)
      (when (string-match "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\)\\'" base)
        (setq base (substring base 0 (match-beginning 0)))
        (setq changed t)))
    base))

(defun org-astro--normalize-downloaded-image-extension (path type)
  "Normalize PATH's extension based on TYPE, returning the final path."
  (if (null type)
      path
    (let* ((ext (org-astro--image-type->extension type))
           (base (file-name-sans-extension path))
           (stripped (org-astro--strip-image-extension-chain base))
           (case-fold-search t))
      (if (and (org-astro--extension-matches-image-type-p path type)
               (string= stripped base))
          path
        (let ((new-path (if ext (concat stripped "." ext) path)))
          (rename-file path new-path t)
          new-path)))))

(defun org-astro--download-with-curl (url target-path)
  "Download URL to TARGET-PATH with curl when available."
  (let ((curl (executable-find "curl")))
    (when curl
      (let ((exit-code (call-process curl nil nil nil
                                     "-L"
                                     "-sS"
                                     "-f"
                                     "--connect-timeout" "10"
                                     "--max-time" "30"
                                     "-H" (concat "User-Agent: " org-astro--remote-image-user-agent)
                                     "-o" target-path
                                     url)))
        (cond
         ((and (eq exit-code 0) (file-exists-p target-path))
          target-path)
         ((file-exists-p target-path)
          (ignore-errors (delete-file target-path))
          nil)
         (t nil))))))

(defun org-astro--download-with-url (url target-path)
  "Download URL to TARGET-PATH using Emacs url.el."
  (let ((url-request-extra-headers `(("User-Agent" . ,org-astro--remote-image-user-agent)
                                     ("Accept" . "image/*,*/*;q=0.8")))
        (url-http-attempt-keepalives nil)
        (url-http-use-global-credentials nil)
        (url-show-status nil)
        (url-user nil)
        (url-user-password nil))
    (url-copy-file url target-path t)
    (when (file-exists-p target-path)
      target-path)))

(defun org-astro--validate-downloaded-image (target-path url)
  "Return plist (:path PATH :type TYPE) when TARGET-PATH looks valid."
  (when (file-exists-p target-path)
    (let* ((data (org-astro--read-file-bytes target-path 4096))
           (size (file-attribute-size (file-attributes target-path)))
           (type (org-astro--detect-image-type-from-bytes data)))
      (cond
       ((or (string-empty-p data)
            (org-astro--data-looks-like-html-p data)
            (and (not type) (< size 1024)))
        (message "[ox-astro][img] Invalid download for %s (size %d)" url size)
        (org-astro--dbg-log nil "REMOTE invalid: %s (size %d)" url size)
        (ignore-errors (delete-file target-path))
        nil)
       (t (list :path target-path :type type))))))

(defun org-astro--download-remote-image (url posts-folder sub-dir)
  "Download remote image from URL to assets folder.
Returns the local file path if successful, nil otherwise."
  (when (and url posts-folder (string-match-p "^https?://" url))
    (let* ((url-parts (url-generic-parse-url url))
           (path (url-filename url-parts))
           ;; Extract filename from URL path, handling query parameters
           (raw-filename (if (string-match "\\([^/?]+\\)\\(\\?.*\\)?$" path)
                             (match-string 1 path)
                             "downloaded-image"))
           ;; Ensure we have an image extension
           (case-fold-search t)
           (filename (if (string-match-p "\\.(png\\|jpe?g\\|jpeg\\|gif\\|webp\\|svg)$" raw-filename)
                         raw-filename
                       (concat raw-filename ".jpg")))
           (clean-filename (org-astro--sanitize-filename filename))
           (assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
           (target-path (when assets-folder
                          (expand-file-name clean-filename assets-folder))))

      (when (and target-path assets-folder)
        ;; Create assets directory if it doesn't exist
        (make-directory assets-folder t)

        ;; Download the image
        (condition-case err
            (progn
              (message "Downloading remote image: %s" url)
              (org-astro--dbg-log nil "REMOTE downloading: %s" url)
              (let* ((downloaded (or (org-astro--download-with-curl url target-path)
                                     (org-astro--download-with-url url target-path)))
                     (validated (and downloaded (org-astro--validate-downloaded-image downloaded url)))
                     (final (when validated
                              (org-astro--normalize-downloaded-image-extension
                               (plist-get validated :path)
                               (plist-get validated :type)))))
                (when final
                  (message "Successfully downloaded: %s -> %s" url final)
                  (org-astro--dbg-log nil "REMOTE downloaded: %s -> %s" url (file-name-nondirectory final))
                  final)))
          (error
           (message "Failed to download image %s: %s" url err)
           (org-astro--dbg-log nil "REMOTE failed: %s - %s" url err)
           nil))))))

(defun org-astro--process-image-path (image-path posts-folder sub-dir &optional update-buffer)
  "Process IMAGE-PATH for POSTS-FOLDER, copying local files or downloading remote URLs to SUB-DIR.
Returns the processed path suitable for Astro imports.
GIFs are copied to public/ folder and return a web path (e.g., /images/...).
If UPDATE-BUFFER is non-nil, updates the current buffer to point to the new path."
  (when (and image-path posts-folder)
    (let* ((image-path (substring-no-properties image-path))
           (is-gif (org-astro--gif-p image-path)))
      (cond
       ;; GIF files go to public/ folder for static serving
       ((and is-gif (file-exists-p image-path))
        (let* ((public-folder (org-astro--get-public-folder posts-folder sub-dir))
               (original-filename (file-name-nondirectory image-path))
               (clean-filename (org-astro--sanitize-filename original-filename))
               (target-path (expand-file-name clean-filename public-folder))
               (web-path (concat "/images/" sub-dir clean-filename)))
          (make-directory public-folder t)
          (condition-case err
              (copy-file image-path target-path t)
            (error (message "[ox-astro][img] Failed to copy GIF %s: %s" image-path err)))
          (when update-buffer
            (org-astro--update-source-buffer-image-path image-path target-path))
          (when (file-exists-p target-path)
            (message "[ox-astro][img] GIF cover image copied to public: %s" web-path)
            web-path)))

       ;; Handle images already in the assets folder - just return the alias path
       ((let ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir)))
          (and assets-folder
               (string-match-p (regexp-quote (expand-file-name assets-folder))
                               (expand-file-name image-path))))
        (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
               (filename (file-name-nondirectory image-path)))
          (concat "~/assets/images/" sub-dir filename)))

       ;; Handle remote URLs (both full https:// and protocol-relative //)
       ((or (string-match-p "^https?://" image-path)
            (and (string-match-p "^//" image-path)
                 (org-astro--image-path-matches-p image-path)))
        (let* ((full-url (if (string-match-p "^//" image-path)
                             (concat "https:" image-path)
                             image-path))
               (downloaded-path (org-astro--download-remote-image full-url posts-folder sub-dir)))
          (when downloaded-path
            (let* ((clean-filename (file-name-nondirectory downloaded-path)))
              ;; If downloaded file is a GIF, move to public
              (if (org-astro--gif-p clean-filename)
                  (let* ((public-folder (org-astro--get-public-folder posts-folder sub-dir))
                         (public-target (expand-file-name clean-filename public-folder))
                         (web-path (concat "/images/" sub-dir clean-filename)))
                    (make-directory public-folder t)
                    (rename-file downloaded-path public-target t)
                    (when update-buffer
                      (let ((original-url (if (string-match-p "^//" image-path) full-url image-path)))
                        (org-astro--update-source-buffer-image-path original-url public-target)))
                    web-path)
                ;; Non-GIF: keep in assets
                (let ((result (concat "~/assets/images/" sub-dir clean-filename)))
                  (when update-buffer
                    (let ((original-url (if (string-match-p "^//" image-path) full-url image-path)))
                      (org-astro--update-source-buffer-image-path original-url downloaded-path)))
                  result))))))

       ;; Handle local files
       (t
        (let* ((assets-folder (org-astro--get-assets-folder posts-folder sub-dir))
               (original-filename (file-name-nondirectory image-path))
               (clean-filename (org-astro--sanitize-filename original-filename))
               (target-path (when assets-folder (expand-file-name clean-filename assets-folder))))

          (when (and target-path (file-exists-p image-path))
            ;; Create assets directory if it doesn't exist
            (when assets-folder
              (make-directory assets-folder t))
            ;; Copy the file
            (condition-case err
                (copy-file image-path target-path t)
              (error (message "[ox-astro][img] Failed to copy %s: %s" image-path err)))

            ;; Update the buffer if requested
            (when update-buffer
              (org-astro--update-source-buffer-image-path image-path target-path))

            ;; Return the alias path for imports
            (when (file-exists-p target-path)
              (concat "~/assets/images/" sub-dir clean-filename)))))))))


(defun org-astro--collect-raw-images-from-tree-region (tree)
  "Collect raw image paths by scanning the buffer region of a parse TREE.
This is more robust for narrowed subtrees than relying on `plain-text` parsing."
  (let (images)
    (let ((beg (org-element-property :begin tree))
          (end (org-element-property :end tree)))
      (when (and beg end)
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (while (re-search-forward "^\\s-*/[^[:space:]]*\\.\\(png\\|jpe?g\\|webp\\)\\s-*$" nil t)
              (let ((path (string-trim (match-string 0))))
                (when (file-exists-p path)
                  (push path images))))))))
    (nreverse images)))
(provide 'ox-astro-image-handlers)

;;; ox-astro-image-handlers.el ends here
