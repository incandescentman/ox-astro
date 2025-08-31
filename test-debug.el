;; Test debug system
(setq org-astro-debug-images t)
(message "Debug mode enabled: %s" org-astro-debug-images)

;; Test direct logging
(org-astro--debug-log-direct "TEST: Debug system initialized")
(org-astro--debug-log-direct "TEST: Testing direct logging at %s" (current-time-string))

;; Check if log file was created
(let ((log-file (expand-file-name "~/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-debug.log")))
  (if (file-exists-p log-file)
      (message "✓ Debug log file created at: %s" log-file)
    (message "✗ Debug log file NOT created")))