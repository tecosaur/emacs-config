#!/usr/bin/env sh
":"; exec script -eqfc "TERM=xterm-direct emacs --quick -nw --eval '(load (expand-file-name \"$0\"))' && rm typescript" 1>/dev/null # -*- mode: emacs-lisp; lexical-binding: t; -*-

(defvar files-to-htmlize '("init.el" "config.el" "packages.el"))
(defvar htmlize-theme 'doom-one-light)

;; Setup

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)))
(initialise)

(defun my-debugger (err &rest debugger-args)
  (message "\033[0;31mERROR: %s\nBACKTRACE: %s\033[0m"
           debugger-args
           (with-temp-buffer
             (let ((standard-output (current-buffer)))
               (backtrace)
               (buffer-string))))
  (kill-emacs 1))
(setq debugger #'my-debugger)

;; Start htmlizing

(require 'htmlize)

(defun my-htmlize-file (file)
  (message "Htmlizing %s" file)
  (let ((output-file (expand-file-name
                      (htmlize-make-file-name (file-name-nondirectory file))
                      (file-name-directory file))))
    (with-current-buffer (find-file-literally file)
      (normal-mode)
      (font-lock-ensure)
      (set-buffer-modified-p nil)
      (with-current-buffer (htmlize-buffer-1)
        (goto-char (point-min))
        (replace-string "</title>\n"
                        "</title>
  <style>
    body > pre {
      font-size: 1rem;
      max-width: min(100rem, 100%);
      width: max-content;
      white-space: pre-wrap;
      margin: auto;
    }
  </style>\n")
        (let ((inhibit-message t))
          (write-file output-file)
          (publish output-file)
          (kill-buffer (current-buffer))))
      (kill-buffer (current-buffer)))))

(load-theme htmlize-theme t)

(dolist (file files-to-htmlize)
  (my-htmlize-file (expand-file-name file config-root)))

(setq inhibit-message t)
(kill-emacs exit-code)
