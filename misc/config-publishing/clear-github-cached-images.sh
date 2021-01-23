#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise)

(let (relevant-lines)
  (with-current-buffer (url-retrieve-synchronously "https://github.com/tecosaur/emacs-config/blob/master/README.org")
    (while (search-forward "data-canonical-src=\"https://tecosaur.github.io/emacs-config/" nil t)
      (push (thing-at-point 'line) relevant-lines)))
  (dolist (line relevant-lines)
    (when-let ((cached-img (when (string-match "href=\"\\([^\"]+\\)\"" line)
                             (match-string 1 line))))
      (let ((url-request-method "PURGE"))
        (message "[36] Asking github to purge %s" cached-img)
        (with-current-buffer
            (url-retrieve-synchronously cached-img)
          (message "%s" (buffer-string)))))))

(setq inhibit-message t)
(kill-emacs exit-code)
