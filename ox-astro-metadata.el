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

(defun org-astro--split-quoted-list (s)
  "Split S by commas/whitespace; preserve items wrapped in quotes.

Separators include commas, spaces, tabs, or newlines. Items wrapped in single
or double quotes retain internal spacing; quotes are stripped in the result."
  (when (and s (stringp s))
    (let* ((len (length s))
           (i 0)
           (quote-char nil)
           (buf (list))
           (items (list))
           (has-comma (string-match-p "," s)))
      (cl-labels ((push-char (c) (setq buf (cons c buf)))
                  (flush-buf ()
                    (when buf
                      (let* ((str (apply #'string (nreverse buf)))
                             (trimmed (org-trim str)))
                        (when (> (length trimmed) 0)
                          (setq items (cons trimmed items))))
                      (setq buf nil))))
        (while (< i len)
          (let ((ch (aref s i)))
            (cond
             (quote-char
              (if (= ch quote-char)
                  (progn
                    (flush-buf)
                    (setq quote-char nil))
                (push-char ch)))
             (t
              (cond
               ((and (null buf) (memq ch '(?\s ?\t ?\n))))
               ((and (null buf) (memq ch '(?\" ?')))
                (setq quote-char ch))
               (((if has-comma
                     (= ch ?,)
                   (memq ch '(?, ?\s ?\t ?\n))))
                (flush-buf))
               (t (push-char ch)))) )
          (setq i (1+ i)))
        (flush-buf)
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

(defun org-astro--keyword-raw-themes (tree)
  "Return raw value for ASTRO_THEMES or THEMES keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_THEMES" "THEMES")))

(defun org-astro--keyword-raw-story-type (tree)
  "Return raw value for ASTRO_STORY_TYPE or STORY_TYPE keyword from TREE."
  (org-astro--keyword-first-value tree '("ASTRO_STORY_TYPE" "STORY_TYPE")))

(defun org-astro--parse-tags (tree info)
  "Return list of tags from TREE/INFO, honoring ASTRO_TAGS and TAGS."
  (let* ((tags-raw (or (org-astro--keyword-raw-tags tree)
                       (plist-get info :astro-tags)
                       (plist-get info :tags))))
    (org-astro--split-quoted-list tags-raw)))

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

(defun org-astro--get-story-type (tree info)
  "Return the story type string from TREE/INFO."
  (let ((story-type-raw (or (org-astro--keyword-raw-story-type tree)
                            (plist-get info :astro-story-type)
                            (plist-get info :story-type))))
    (when story-type-raw
      (let ((trimmed (org-trim story-type-raw)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(provide 'ox-astro-metadata)
;;; ox-astro-metadata.el ends here
