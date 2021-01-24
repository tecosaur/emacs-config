#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise)

;;; Actually do the exporting now

(message "[34] Opening config file: %s"
         (expand-file-name "config.org" config-root))

(require 'vc) ; need this for modification-time macro

(setq org-mode-hook nil)
(with-current-buffer (find-file-noselect (expand-file-name "config.org" config-root))
  (message "[33] Exporting %s" (buffer-file-name))
  (org-html-export-to-html))

(publish "config.html" "misc/*.svg")

(make-symbolic-link (expand-file-name "config.html" publish-dir)
                    (expand-file-name "index.html" publish-dir))

(message "[1;32] Config export complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
