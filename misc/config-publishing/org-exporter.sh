#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq message-colour t)
(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise)

;;; Actually do the exporting now

(message "[34] Opening config file: %s"
         (expand-file-name "config.org" config-root))

(require 'vc) ; need this for modification-time macro

(with-current-buffer (find-file (expand-file-name "config.org" config-root))
  (message "[33] Exporting %s" (buffer-file-name))
  (org-html-export-to-html))

(make-symbolic-link (expand-file-name "config.html" config-root)
                    (expand-file-name "index.html" config-root))

(publish "config.html" "index.html" "misc/*.svg")

(message "[1;32] Config export complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
