#!/bin/bash
cd /Users/jay/Library/CloudStorage/Dropbox/github/ox-astro
emacs --batch \
  --eval "(setq load-path (cons \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro\" load-path))" \
  --eval "(require 'org)" \
  --eval "(load \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-config.el\")" \
  --eval "(load \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-helpers.el\")" \
  --eval "(load \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro-handlers.el\")" \
  --eval "(load \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/ox-astro.el\")" \
  --eval "(find-file \"/Users/jay/Library/CloudStorage/Dropbox/github/ox-astro/test-underscore-minimal.org\")" \
  --eval "(let ((completing-read-function (lambda (prompt collection &rest args) \"jaydocs\"))) (org-astro-export-to-mdx))"