;;; ------------------------------------------------------------------
;;; CUT HERE FOR ox-astro-config.el
;;; ------------------------------------------------------------------
(defgroup org-export-astro nil
  "Options for exporting Org mode files to Astro-compatible MDX."
  :tag "Org Export Astro"
  :group 'org-export
  :version "26.3")

(defcustom org-astro-date-format "%Y-%m-%dT%H:%M:%SZ"
  "Date format used for exporting 'publishDate' in front-matter."
  :group 'org-export-astro
  :type 'string)

(defcustom org-astro-default-posts-folder "astro-posts"
  "Default subdirectory for exported posts if #+DESTINATION_FOLDER, #+DESTINATION-FOLDER, or #+DESTINATION is not set."
  :group 'org-export-astro
  :type 'string)

(defcustom org-astro-default-author-image
  "~/assets/images/authors/jay-dixit-512.png"   ;; <─ only this line changed
  "Default author image path if not specified in the Org file.
Uses Astro's alias, which maps to the project's src/ directory."
  :group 'org-export-astro
  :type 'string)   ;; treat it as raw front-matter text, not a local file

(defcustom org-astro-debug-images nil
  "When non-nil, log detailed image-handling diagnostics and emit MDX comments."
  :group 'org-export-astro
  :type 'boolean)

(defcustom org-astro-image-default-layout "responsive"
  "Default layout prop for generated <Image /> components.
Astro 5.10+ uses this to automatically generate srcset and sizes.

Valid values:
- \"responsive\" (recommended): Image scales with container, auto srcset
- \"fixed\": Image maintains exact dimensions
- \"full-width\": Image spans full container width
- nil or \"none\": No layout prop added (basic behavior)

Set to nil to disable responsive images globally."
  :group 'org-export-astro
  :type '(choice (const :tag "Responsive (recommended)" "responsive")
                 (const :tag "Fixed" "fixed")
                 (const :tag "Full width" "full-width")
                 (const :tag "None (basic)" nil)))

(defcustom org-astro-source-root-folder
  "~/org-files"
  "Root directory that contains your source Org files.
Used for calculating relative paths when preserving folder structure."
  :group 'org-export-astro
  :type 'directory)

(defcustom org-astro-id-link-base-path nil
  "Base path for org-roam ID links in exported MDX.
When non-nil, ID links are rendered as absolute routes: BASE-PATH/collection-id
Example: \"/notes\" produces links like \"/notes/stories/my-story\"

When nil (default), ID links are rendered as relative MDX paths:
\"../stories/my-story.mdx\"

Set this to match your Astro routing structure. For example:
- life-web uses \"/notes\" (pages at /notes/[...id].astro)
- socratic uses nil (relative links within content collection)"
  :group 'org-export-astro
  :type '(choice (const :tag "Relative MDX paths" nil)
                 (string :tag "Absolute route base path")))

(defcustom org-astro-known-posts-folders
  '(("blog"       . (:path "~/projects/my-astro-site/src/content/blog"))
    ("docs"       . (:path "~/projects/my-docs-site/src/content/docs"))
    ("socratic"   . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socratic/src/content/blog"))
    ("socratic-studies" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socratic/src/content/studies"
                          :frontmatter studies))
    ("jaydocs"    . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog"))
    ("life-web"   . (:path "/Users/jay/Dropbox/github/life-web/src/content/notes"
                     :preserve-folder-structure t))
    ("roam-life"  . (:path "/Users/jay/Dropbox/github/life-web/src/content/notes"
                     :preserve-folder-structure t))
    )
  "An alist of known directories for exporting Astro posts.
Each element is (NICKNAME . PLIST) where PLIST can contain:
  :path - The destination directory path (required)
  :preserve-folder-structure - When t, preserve source folder structure (optional)

To customize this:
1. Run M-x customize-group RET org-export-astro RET
2. Edit the 'Org Astro Known Posts Folders' setting
3. Add your project paths using nicknames you can reference in #+DESTINATION_FOLDER, #+DESTINATION-FOLDER, or #+DESTINATION

Example configuration:
  ((\"blog\" . (:path \"/path/to/my-blog/src/content/blog\"))
   (\"docs\" . (:path \"/path/to/my-docs/src/content/docs\"))
   (\"roam\" . (:path \"/path/to/roam/src/content\"
               :preserve-folder-structure t)))

Then in your Org files, use: #+DESTINATION_FOLDER: blog (or #+DESTINATION-FOLDER: blog, or #+DESTINATION: blog)"
  :group 'org-export-astro
  :type '(alist :key-type (string :tag "Nickname")
                :value-type (plist :options ((:path directory)
                                             (:preserve-folder-structure boolean)))))

;; Extend org-emphasis-regexp-components to recognize em-dashes as valid
;; boundaries for emphasis markers like /italics/ and *bold*.
;; This allows: "text—/italics/—more" to render correctly.
(with-eval-after-load 'org
  (when (boundp 'org-emphasis-regexp-components)
    (let ((pre  (nth 0 org-emphasis-regexp-components))
          (post (nth 1 org-emphasis-regexp-components)))
      ;; Add em-dash (—) and en-dash (–) to pre and post match characters
      (unless (string-match-p "—" pre)
        (setcar (nthcdr 0 org-emphasis-regexp-components)
                (concat "—–" pre)))
      (unless (string-match-p "—" post)
        (setcar (nthcdr 1 org-emphasis-regexp-components)
                (concat "—–" post)))
      ;; Rebuild the emphasis regex
      (org-set-emph-re 'org-emphasis-regexp-components
                       org-emphasis-regexp-components))))

(provide 'ox-astro-config)

;;; ------------------------------------------------------------------
;;; END CUT FOR ox-astro-config.el
;;;
;;; This file should contain all defgroup and defcustom definitions.
;;; ------------------------------------------------------------------
