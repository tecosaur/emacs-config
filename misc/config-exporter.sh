#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq start-time (float-time))
(message "\033[1;0mBuilding config file export\033[0;90m")

(defun timed-message (orig-fn format-str &rest args)
  (if (string-match-p "[%\\.1fs]" format-str)
      (apply orig-fn format-str args)
    (apply orig-fn
           (concat "[%.1fs] " format-str)
           (append (list (- (float-time) start-time))
                   args))))
(advice-add 'message :around #'timed-message)

(setq debug-on-error t
      doom-debug-p t)

(advice-add 'theme-magic-from-emacs :override #'ignore)
(advice-add 'format-all-buffer :override #'ignore)
(load (expand-file-name "~/.emacs.d/init.el"))

(setq exit-code 0)
(defun red-error (orig-fn &rest args)
  (message "\033[0;31m")
  (apply orig-fn args)
  (message "\033[0m")
  (setq exit-code 1))
(advice-add 'debug :around #'red-error)

(message "\033[0;34m[%.1fs] Opening config file: %s\033[90m"
         (- (float-time) start-time)
         (expand-file-name "../config.org"))

(require 'vc) ; need this for modification-time macro

(with-current-buffer (find-file (expand-file-name  "../config.org"))
  (message "\033[0;33m[%.1fs] Exporting %s\033[0m"
           (- (float-time) start-time)
           (buffer-file-name))
  (org-html-export-to-html))

(message "\033[0;33m[%.1fs] Htmlizing %s\033[0m"
         (- (float-time) start-time)
         (buffer-file-name))
(htmlize-file (expand-file-name  "../init.el"))
(htmlize-file (expand-file-name  "../config.el"))
(htmlize-file (expand-file-name  "../packages.el"))

(message "\033[1;32m[%.1fs] Config export complete!\033[0m"
         (- (float-time) start-time))

(setq inhibit-message t)
(kill-emacs exit-code)
