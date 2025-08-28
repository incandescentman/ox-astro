;;; test-conversion.el -- Test image conversion logic
(require 'cl-lib)
(setq load-path (cons "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro" load-path))
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-config.el")
(load "/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-helpers.el")

;; Simulate the import data as it would exist after hero exclusion
(setq test-imports 
  '((:path "/Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg" 
     :var-name "img502610958245334564829239741694898706627993310N")
    (:path "/Users/jay/Downloads/502963633_24533456719590617_6999624331092055355_n.jpg"
     :var-name "img502963633245334567195906176999624331092055355N") 
    (:path "/Users/jay/Downloads/Rachide-News-01(1).jpg"
     :var-name "RachideNews011")
    (:path "/Users/jay/Downloads/masimo-dutti.webp"
     :var-name "masimoDutti")
    (:path "/Users/jay/Downloads/2017021468194489.webp"
     :var-name "img2017021468194489")))

;; Test paths to convert  
(setq test-paths '("/Users/jay/Downloads/502610958_24533456482923974_1694898706627993310_n.jpg"
                   "/Users/jay/Downloads/502963633_24533456719590617_6999624331092055355_n.jpg" 
                   "/Users/jay/Downloads/Rachide-News-01(1).jpg"
                   "/Users/jay/Downloads/2017021468194489.webp"))

(message "Testing import matching:")
(dolist (path test-paths)
  (let ((found-import (cl-find path test-imports
                               :key (lambda (item) (plist-get item :path))
                               :test #'string-equal)))
    (message "Path: %s" path)
    (if found-import
        (message "  Found import: %s" (plist-get found-import :var-name))
      (message "  NOT FOUND in imports"))
    (message "")))