;;; debug-regex.el -- Test regex patterns
(require 'cl-lib)

;; Test paths
(setq test-paths '(
  "/Users/jay/Downloads/masimo-dutti.webp"
  "/Users/jay/Downloads/106910784_3615490375147223_8376969077115059792_n.jpg"
  "/Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg"  
  "/Users/jay/Downloads/502963633_24533456719590617_6999624331092055355_n.jpg"
  "/Users/jay/Downloads/Rachide-News-01(1).jpg"
  "/Users/jay/Downloads/2017021468194489.webp"
))

;; Test regex patterns
(setq old-pattern "^/.*\\.\(png\\|jpe?g\\|webp\)$")
(setq new-pattern "^/.*\\.(png\\|jpe?g\\|webp)$")

(message "Testing regex patterns:")
(dolist (path test-paths)
  (let ((old-match (string-match-p old-pattern path))
        (new-match (string-match-p new-pattern path)))
    (message "Path: %s" path)
    (message "  Old pattern: %s" (if old-match "MATCH" "NO MATCH"))
    (message "  New pattern: %s" (if new-match "MATCH" "NO MATCH"))
    (message "")))

;; Test specific function behavior
(message "Testing org-trim behavior:")
(setq test-lines '(
  "/Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg"
  " /Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg "
  "/Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg\n"
))

(dolist (line test-lines)
  (let ((trimmed (org-trim line)))
    (message "Original: %S" line)
    (message "Trimmed:  %S" trimmed)
    (message "Match:    %s" (if (string-match-p new-pattern trimmed) "MATCH" "NO MATCH"))
    (message "")))