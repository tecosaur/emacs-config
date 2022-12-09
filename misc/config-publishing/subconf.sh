#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (expand-file-name (format "%s-log.txt" (file-name-base load-file-name))))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise 'light)

;;; Actually do the exporting now

(message "[34] Publishing raw subconf files")

(publish "subconf/*.el")

;;; Engraving

;; For some reason, these seem to behave a bit strangely.
(add-load-path! "~/.config/emacs/.local/straight/repos/parent-mode/"
                "~/.config/emacs/.local/straight/repos/highlight-quoted/")

(require 'highlight-numbers)
(require 'highlight-quoted)
(require 'rainbow-delimiters)
(require 'engrave-faces-html)

(load (expand-file-name "doom-one-light-engraved-theme.el"))
(engrave-faces-use-theme 'doom-one-light)

(dolist (subconf-file (directory-files (expand-file-name "subconf" config-root) t "\\.el$"))
  (engrave-faces-html-file subconf-file))

(publish '("subconf/*.html" . "engraved"))

(message "[1;32] Subconf publish complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
