;;; ox-astro-metadata.el --- Extended metadata helpers for ox-astro -*- lexical-binding: t -*-

;;; Commentary:
;; Utility functions for parsing extended front-matter attributes such as
;; eras, places, themes, and story types.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'cl-lib)

(declare-function org-astro--format-date "ox-astro-helpers")
(declare-function org-astro--debug-log-direct "ox-astro-helpers")

(defun org-astro--clean-tag (tag)
  "Return a sanitized TAG string or nil if invalid.
Strips whitespace, drops org-capture template markers, and ignores
empty or whitespace-only values."
  (cond
   ((null tag) nil)
   ((stringp tag)
    (let ((trimmed (string-trim tag)))
      (cond
       ((string-empty-p trimmed) nil)
       ((string-match-p "^%\\^{" trimmed) nil)
       (t trimmed))))
   ((numberp tag) (number-to-string tag))
   (t nil)))

(defun org-astro--split-quoted-list (s)
  "Split S by commas/whitespace; preserve items wrapped in quotes.

Separators include commas, spaces, tabs, or newlines. Items wrapped in single
or double quotes retain internal spacing; quotes are stripped in the result."
  (when (and s (stringp s))
    (let* ((len (length s))
           (i 0)
           (quote-char nil)
           (token nil)
           (items nil)
           (has-comma (string-match-p "," s)))
      (cl-labels ((push-char (c) (setq token (cons c token)))
                  (finish-token ()
                    (when token
                      (let* ((str (apply #'string (nreverse token)))
                             (trimmed (org-trim str)))
                        (when (> (length trimmed) 0)
                          (setq items (cons trimmed items))))
                      (setq token nil))))
        (while (< i len)
          (let ((ch (aref s i)))
            (if quote-char
                (if (= ch quote-char)
                    (progn
                      (finish-token)
                      (setq quote-char nil))
                  (push-char ch))
              (cond
               ((memq ch '(?\s ?\t ?\n))
                (if has-comma
                    ;; In comma mode, keep internal spaces (trim later) but
                    ;; ignore leading whitespace before a token starts.
                    (when token (push-char ch))
                  (finish-token)))
               ((= ch ?,)
                (finish-token))
               ((and (null token) (memq ch '(?\" ?')))
                (setq quote-char ch))
               (t
                (push-char ch)))))
          (setq i (1+ i)))
        (finish-token)
        (nreverse items)))))

(defun org-astro--keyword-first-value (tree keys)
  "Return the first keyword value in TREE whose key is listed in KEYS."
  (org-element-map tree 'keyword
    (lambda (k)
      (when (member (org-element-property :key k) keys)
        (org-element-property :value k)))
    nil 'first-match))

(defun org-astro--keyword-raw-date-occurred (tree)
  "Return raw value for ASTRO_DATE_OCCURRED or DATE_OCCURRED."
  (org-astro--keyword-first-value tree '("ASTRO_DATE_OCCURRED" "DATE_OCCURRED")))

(defun org-astro--keyword-raw-era (tree)
  "Return raw value for ASTRO_ERA or ERA keyword."
  (org-astro--keyword-first-value tree '("ASTRO_ERA" "ERA")))

(defun org-astro--keyword-raw-tags (tree)
  "Return raw value for ASTRO_TAGS or TAGS keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_TAGS" "TAGS")))

(defun org-astro--keyword-raw-categories (tree)
  "Return raw value for ASTRO_CATEGORIES or CATEGORIES keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_CATEGORIES" "CATEGORIES")))

(defun org-astro--keyword-raw-places (tree)
  "Return raw value for ASTRO_PLACES or PLACES keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_PLACES" "PLACES")))

(defun org-astro--keyword-raw-place (tree)
  "Return raw value for ASTRO_PLACE or PLACE keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_PLACE" "PLACE")))

(defun org-astro--keyword-raw-themes (tree)
  "Return raw value for ASTRO_THEMES or THEMES keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_THEMES" "THEMES")))

(defun org-astro--keyword-raw-story-type (tree)
  "Return raw value for ASTRO_STORY_TYPE or STORY_TYPE keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_STORY_TYPE" "STORY_TYPE")))

(defun org-astro--keyword-raw-people (tree)
  "Return raw value for ASTRO_PEOPLE or PEOPLE keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_PEOPLE" "PEOPLE")))

(defun org-astro--keyword-raw-emotions (tree)
  "Return raw value for ASTRO_EMOTIONS or EMOTIONS keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_EMOTIONS" "EMOTIONS")))

(defun org-astro--keyword-raw-media (tree)
  "Return raw value for ASTRO_MEDIA or MEDIA keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_MEDIA" "MEDIA")))

(defun org-astro--keyword-raw-incomplete (tree)
  "Return raw value for ASTRO_INCOMPLETE or INCOMPLETE keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_INCOMPLETE" "INCOMPLETE")))

(defun org-astro--keyword-raw-hero-credit (tree)
  "Return raw value for HERO-CREDIT keyword from TREE."
  (org-astro--keyword-first-value tree '("HERO-CREDIT" "HERO_CREDIT")))

(defun org-astro--keyword-raw-hero-caption (tree)
  "Return raw value for HERO-CAPTION keyword from TREE."
  (org-astro--keyword-first-value tree '("HERO-CAPTION" "HERO_CAPTION")))

(defun org-astro--keyword-raw-connection (tree base)
  "Return raw value for connection keyword BASE (e.g., \"CONNECTION_TEMPORAL\")."
  (org-astro--keyword-first-value tree (list (format "ASTRO_%s" base) base)))

(defun org-astro--parse-tags (tree info)
  "Return list of tags from TREE/INFO, honoring ASTRO_TAGS and TAGS."
  (let* ((tags-raw (or (org-astro--keyword-raw-tags tree)
                       (plist-get info :astro-tags)
                       (plist-get info :tags)))
         (filetags (plist-get info :filetags))
         (parsed (org-astro--split-quoted-list tags-raw))
        (from-filetags (when filetags
                         (mapcar (lambda (tag)
                                   (let ((name (org-no-properties tag)))
                                     (if (string-prefix-p ":" name)
                                         (string-trim name ":")
                                       name)))
                                 filetags))))
    (cl-remove-duplicates
     (delq nil (mapcar #'org-astro--clean-tag (append parsed from-filetags)))
     :test #'string=)))

(defun org-astro--parse-categories (tree info)
  "Return list of categories from TREE/INFO, honoring ASTRO_CATEGORIES."
  (let* ((categories-raw (or (org-astro--keyword-raw-categories tree)
                             (plist-get info :astro-categories)
                             (plist-get info :categories))))
    (org-astro--split-quoted-list categories-raw)))

(defun org-astro--parse-places (tree info)
  "Return list of places from TREE/INFO, honoring ASTRO_PLACES and PLACES."
  (let* ((places-raw (or (org-astro--keyword-raw-places tree)
                         (plist-get info :astro-places)
                         (plist-get info :places))))
    (org-astro--split-quoted-list places-raw)))

(defun org-astro--parse-themes (tree info)
  "Return list of themes from TREE/INFO, honoring ASTRO_THEMES and THEMES."
  (let* ((themes-raw (or (org-astro--keyword-raw-themes tree)
                         (plist-get info :astro-themes)
                         (plist-get info :themes))))
    (org-astro--split-quoted-list themes-raw)))

(defun org-astro--parse-people (tree info)
  "Return list of people from TREE/INFO, honoring ASTRO_PEOPLE and PEOPLE."
  (let* ((people-raw (or (org-astro--keyword-raw-people tree)
                         (plist-get info :astro-people)
                         (plist-get info :people))))
    (org-astro--split-quoted-list people-raw)))

(defun org-astro--parse-emotions (tree info)
  "Return list of emotions from TREE/INFO, honoring ASTRO_EMOTIONS and EMOTIONS."
  (let* ((emotions-raw (or (org-astro--keyword-raw-emotions tree)
                           (plist-get info :astro-emotions)
                           (plist-get info :emotions))))
    (org-astro--split-quoted-list emotions-raw)))

(defun org-astro--parse-media (tree info)
  "Return list of media attachments from TREE/INFO."
  (let* ((media-raw (or (org-astro--keyword-raw-media tree)
                        (plist-get info :astro-media)
                        (plist-get info :media))))
    (cond
     ((listp media-raw)
      (cl-remove-if (lambda (item) (and (stringp item) (string-empty-p item)))
                    media-raw))
     (t (org-astro--split-quoted-list media-raw)))))

(defun org-astro--parse-incomplete (tree info)
  "Return :true, :false, or nil depending on INCOMPLETE keyword presence."
  (let ((raw (or (org-astro--keyword-raw-incomplete tree)
                 (plist-get info :astro-incomplete)
                 (plist-get info :incomplete))))
    (cond
     ((null raw) nil)
     ((eq raw t) :true)
     ((eq raw :true) :true)
     ((eq raw :false) :false)
     ((and (stringp raw)
           (let ((trimmed (downcase (org-trim raw))))
             (cond
              ((or (string-empty-p trimmed)
                   (member trimmed '("true" "t" "yes" "y" "1")))
               :true)
              ((member trimmed '("false" "f" "no" "n" "0"))
               :false)
              (t :true)))))
     (t :true))))

(defun org-astro--parse-connections (tree info)
  "Return an alist of connection lists extracted from TREE/INFO."
  (let* ((temporal-raw (or (org-astro--keyword-raw-connection tree "CONNECTION_TEMPORAL")
                           (plist-get info :astro-connection-temporal)
                           (plist-get info :connection-temporal)))
         (emotional-raw (or (org-astro--keyword-raw-connection tree "CONNECTION_EMOTIONAL")
                            (plist-get info :astro-connection-emotional)
                            (plist-get info :connection-emotional)))
         (thematic-raw (or (org-astro--keyword-raw-connection tree "CONNECTION_THEMATIC")
                           (plist-get info :astro-connection-thematic)
                           (plist-get info :connection-thematic)))
         (temporal (cond
                    ((listp temporal-raw)
                     (cl-remove-if (lambda (item) (and (stringp item) (string-empty-p item)))
                                   temporal-raw))
                    (t (org-astro--split-quoted-list temporal-raw))))
         (emotional (cond
                     ((listp emotional-raw)
                      (cl-remove-if (lambda (item) (and (stringp item) (string-empty-p item)))
                                    emotional-raw))
                     (t (org-astro--split-quoted-list emotional-raw))))
         (thematic (cond
                    ((listp thematic-raw)
                     (cl-remove-if (lambda (item) (and (stringp item) (string-empty-p item)))
                                   thematic-raw))
                    (t (org-astro--split-quoted-list thematic-raw))))
         (result nil))
    (when temporal
      (setq result (cons (cons 'temporal temporal) result)))
    (when emotional
      (setq result (cons (cons 'emotional emotional) result)))
    (when thematic
      (setq result (cons (cons 'thematic thematic) result)))
    (nreverse result)))

(defun org-astro--get-date-occurred (tree info)
  "Extract and format the `dateOccurred' value from TREE/INFO."
  (let ((date-raw (or (org-astro--keyword-raw-date-occurred tree)
                      (plist-get info :astro-date-occurred)
                      (plist-get info :date-occurred))))
    (when (and date-raw (not (string-empty-p (org-trim date-raw))))
      (let ((trimmed (org-trim date-raw)))
        (condition-case err
            (org-astro--format-date trimmed info)
          (error
           (org-astro--debug-log-direct "DATE_OCCURRED parse error for %s: %s"
                                        trimmed err)
           trimmed))))))

(defun org-astro--get-era (tree info)
  "Return the era string from TREE/INFO."
  (let ((era-raw (or (org-astro--keyword-raw-era tree)
                     (plist-get info :astro-era)
                     (plist-get info :era))))
    (when era-raw
      (let ((trimmed (org-trim era-raw)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun org-astro--get-place (tree info)
  "Return the place string from TREE/INFO."
  (let ((place-raw (or (org-astro--keyword-raw-place tree)
                       (plist-get info :astro-place)
                       (plist-get info :place))))
    (when place-raw
      (let ((trimmed (org-trim place-raw)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun org-astro--get-story-type (tree info)
  "Return the story type string from TREE/INFO."
  (let ((story-type-raw (or (org-astro--keyword-raw-story-type tree)
                            (plist-get info :astro-story-type)
                            (plist-get info :story-type))))
    (when story-type-raw
      (let* ((trimmed (org-trim story-type-raw))
             (valid '("full-story" "vignette" "snapshot" "fragment" "conversation")))
        (cond
         ((string-empty-p trimmed) nil)
         ((member trimmed valid) trimmed)
         (t
          (org-astro--debug-log-direct
           "Invalid STORY_TYPE '%s'. Expected one of %s"
           trimmed valid)
          nil))))))

(defun org-astro--get-hero-credit (tree info)
  "Return the hero image credit string from TREE/INFO."
  (let ((credit-raw (or (org-astro--keyword-raw-hero-credit tree)
                        (plist-get info :hero-credit))))
    (when credit-raw
      (let ((trimmed (org-trim credit-raw)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun org-astro--get-hero-caption (tree info)
  "Return the hero image caption string from TREE/INFO."
  (let ((caption-raw (or (org-astro--keyword-raw-hero-caption tree)
                         (plist-get info :hero-caption))))
    (when caption-raw
      (let ((trimmed (org-trim caption-raw)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(provide 'ox-astro-metadata)
;;; ox-astro-metadata.el ends here
