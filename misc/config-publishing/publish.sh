#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq default-directory (file-name-directory load-file-name))

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el") nil t)

(message "Starting publish process")

;; Do the things

(load (expand-file-name "async-proc-management.el") nil t)

(apm-exec '("htmlize.sh")) ; "check-package-updates.sh"))

(if (not (file-exists-p (concat user-emacs-directory "xkcd/")))
    (apm-exec "org-html.sh" :then "org-pdf.sh")
  (apm-exec '("org-html.sh" "org-pdf.sh")))

(apm-wait-and-monitor)

(if (= 0 exit-code)
    (message "[1;32] Config publish content generated!%s" (apm-space-fill-line 33))
  (message "[1;31] Config publishing aborted%s" (apm-space-fill-line 25)))

(setq inhibit-message t)
(kill-emacs exit-code)
