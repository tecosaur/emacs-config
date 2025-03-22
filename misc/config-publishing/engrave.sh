#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (expand-file-name (format "%s-log.txt" (file-name-base load-file-name))))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise 'light)

;;; Actually do the exporting now

(message "[34] Engraving files")

(require 'highlight-numbers)
(require 'highlight-quoted)
(require 'rainbow-delimiters)
(require 'engrave-faces-html)

(load (expand-file-name "doom-one-light-engraved-theme.el"))
(engrave-faces-use-theme 'doom-one-light)

(defvar files-to-engrave
  '("init.el" "config.el" "packages.el" "config.org"))

(when (member "org" (mapcar #'file-name-extension files-to-engrave))
  (when (assoc 'org after-load-alist)
    (setcdr (assoc 'org after-load-alist) nil))
  (setq org-load-hook nil)
  (require 'org)
  (require 'org-persist)
  (remove-hook 'kill-emacs-hook #'org-persist-gc)
  (setq org-mode-hook nil))

(let ((default-directory config-root))
  (dolist (file files-to-engrave)
    (engrave-faces-html-file file)))

(publish '("*.html" . "engraved"))

(message "[1;32] Engraving complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
