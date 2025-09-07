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
  "Default subdirectory for exported posts if #+DESTINATION_FOLDER is not set."
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

(defcustom org-astro-source-root-folder
  "~/org-files"
  "Root directory that contains your source Org files.
Used for calculating relative paths when preserving folder structure."
  :group 'org-export-astro
  :type 'directory)

(defcustom org-astro-known-posts-folders
  '(("blog"       . (:path "~/projects/my-astro-site/src/content/blog"))
    ("docs"       . (:path "~/projects/my-docs-site/src/content/docs"))
    ("socraticai" . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/socraticai/src/content/blog"))
    ("jaydocs"    . (:path "/Users/jay/Library/CloudStorage/Dropbox/github/astro-monorepo/apps/jaydocs/src/content/blog"))
    )
  "An alist of known directories for exporting Astro posts.
Each element is (NICKNAME . PLIST) where PLIST can contain:
  :path - The destination directory path (required)
  :preserve-folder-structure - When t, preserve source folder structure (optional)

To customize this:
1. Run M-x customize-group RET org-export-astro RET
2. Edit the 'Org Astro Known Posts Folders' setting
3. Add your project paths using nicknames you can reference in #+DESTINATION_FOLDER

Example configuration:
  ((\"blog\" . (:path \"/path/to/my-blog/src/content/blog\"))
   (\"docs\" . (:path \"/path/to/my-docs/src/content/docs\"))
   (\"roam\" . (:path \"/path/to/roam/src/content\" 
               :preserve-folder-structure t)))

Then in your Org files, use: #+DESTINATION_FOLDER: blog"
  :group 'org-export-astro
  :type '(alist :key-type (string :tag "Nickname")
                :value-type (plist :options ((:path directory)
                                             (:preserve-folder-structure boolean)))))

(provide 'ox-astro-config)

;;; ------------------------------------------------------------------
;;; END CUT FOR ox-astro-config.el
;;;
;;; This file should contain all defgroup and defcustom definitions.
;;; ------------------------------------------------------------------
