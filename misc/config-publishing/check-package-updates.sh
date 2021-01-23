#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq message-colour t)
(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise)

;;; Generation of status badge

(defun gen-status-img (upgradeable total)
  (let ((colour (pcase (/ (float upgradeable) total)
                  ((pred (= 0.0)) "brightgreen")
                  ((pred (> 0.05)) "green")
                  ((pred (> 0.15)) "yellowgreen")
                  ((pred (> 0.3)) "yellow")
                  ((pred (> 0.6)) "orange")
                  (_ "red")))
        (text (if (= 0 upgradeable)
                  (format "all %s up to date" total)
                (format "%s/%s up to date" (- total upgradeable) total))))
    (call-process
     "wget"
     nil nil nil
     "-O"
     "../pkg-status.svg"
     (format "https://img.shields.io/badge/packages-%s-%s.svg?style=flat-square"
             (url-hexify-string text) colour))
    (publish "misc/pkg-status.svg")))

;;; Do it

(message "[34] Opening package file")

(with-current-buffer (find-file (expand-file-name "packages.el" config-root))
  (message "[34] Checking for updates")
  (setq package-upgrades (doom/bump-packages-in-buffer))
  (goto-char (point-min))
  (setq total-packages 0)
  (while (search-forward "(package!" nil t)
    (setq total-packages (1+ total-packages)))
  (kill-buffer (current-buffer)))

(message "[32] %s total packages" total-packages)

(setq upgradeable-packages
      (if (string= "No packages to update" package-upgrades) 0
        (1- (length (split-string package-upgrades "\n")))))

(message "[33] %s packages upgradable" upgradeable-packages)

(gen-status-img upgradeable-packages total-packages)

(when (> upgradeable-packages 0)
  (write-region
   (replace-regexp-in-string
    "- \\[ *[0-9.s]+\\] " ""
    (replace-regexp-in-string
     "\033[0;90m" ""
     (replace-regexp-in-string
      "\\`.*\n" ""
      package-upgrades)))
   nil
   (expand-file-name "misc/upgradable-packages.txt" config-root))
  (publish "misc/upgradable-packages.txt"))

(setq inhibit-message t)
(kill-emacs exit-code)
