#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise 'light)

(doom-require 'doom-lib 'packages)
(doom-require 'doom-lib 'text)

  ;;; Generation of status badge

(defun gen-status-img (upgradeable total)
  (let ((colour (pcase (/ (float upgradeable) total)
                  ((pred (= 0.0)) "brightgreen")
                  ((pred (> 0.15)) "green")
                  ((pred (> 0.25)) "yellowgreen")
                  ((pred (> 0.4)) "yellow")
                  ((pred (> 0.8)) "orange")
                  (_ "red")))
        (text (if (= 0 upgradeable)
                  (format "all %s up to date" total)
                (format "%s/%s up to date" (- total upgradeable) total))))
    (call-process
     "wget"
     nil nil nil
     "-O"
     (expand-file-name "misc/pkg-status.svg" config-root)
     (format "https://img.shields.io/badge/packages-%s-%s.svg?style=flat-square&logo=blueprint"
             (url-hexify-string text) colour))
    (publish "misc/pkg-status.svg")))

  ;;; Show commits

(setq upgradeable-packages 0)

(defadvice! doom/bump-package-at-point-more-detail (&optional select)
  "Include commit messages."
  :override #'doom/bump-package-at-point
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist beg end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (when (or (memq package doom-disabled-packages)
              (looking-back "^[ 	]*;.*" (line-beginning-position)))
      (user-error "Package %s is disabled, skipping." package))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                  (straight-vc-git--encode-url upstream-repo upstream-host)))
           (id (or (when url
                     (cdr (doom-call-process
                           "git" "ls-remote" url
                           (unless select branch))))
                   (user-error "Couldn't find a recipe for %s" package)))
           (id (car (split-string
                     (if select
                         (completing-read "Commit: " (split-string id "\n" t))
                       id))))
           (commits (unless (and oldid
                                 (plist-member plist :pin)
                                 (equal oldid id))
                      (let ((default-directory
                             (or (when (plist-member recipe :local-repo)
                                   (expand-file-name (plist-get recipe :local-repo) doom-private-dir))
                                 (straight--repos-dir
                                  (file-name-sans-extension
                                   (file-name-nondirectory url))))))
                        (doom-call-process "git" "fetch")
                        (concat "  "
                                (cdr
                                 (doom-call-process
                                  "git" "log" "--pretty=format:  %h %s"
                                  (format "%s...%s" oldid id))))))))
      (when (and oldid (equal oldid id))
        (user-error "%s: no update necessary" package))
      (save-excursion
        (if (re-search-forward ":pin +\"\\([^\"]+\\)\"" end t)
            (replace-match id t t nil 1)
          (goto-char (1- end))
          (insert " :pin " (prin1-to-string id))))
      (cond ((not oldid)
             (message "%s: → %s" package (substring id 0 10)))
            ((< (length oldid) (length id))
             (message "%s: extended to %s..." package id))
            (t
             (setq upgradeable-packages (1+ upgradeable-packages))
             (message "%s: %s → %s\n%s"
                      package
                      (substring oldid 0 10)
                      (substring id 0 10)
                      commits))))))

  ;;; Do it

(message "[34] Opening package file")

(setq packages nil)

(with-temp-buffer
  (setq buffer-file-name (expand-file-name "packages.el" config-root))
  (insert-file-contents buffer-file-name)
  (emacs-lisp-mode)
  ;; Running `doom/bump-package-at-point' on my Org statement
  ;; causes it to hang, so let's get rid of it.
  (goto-char (point-min))
  (when (search-forward "\n(package! org" nil t)
    (search-backward "(")
    (delete-region
     (point)
     (save-excursion
       (forward-sexp 1)
       (point))))
  (message "[34] Checking for updates")
  (pp packages)
  (setq package-upgrades
        (doom/bump-packages-in-buffer))
  (goto-char (point-min))
  (setq total-packages 0)
  (while (search-forward "\n(package!" nil t)
    (setq total-packages (1+ total-packages))))

(message "[32] %s total packages" total-packages)
(message "[33] %s packages upgradable" upgradeable-packages)

(gen-status-img upgradeable-packages total-packages)

(when (> upgradeable-packages 0)
  (write-region
   (replace-regexp-in-string
    "- \\[ *[0-9.s]+\\] " ""
    (replace-regexp-in-string
     "\033\\[0;90m" ""
     (replace-regexp-in-string
      "\\`.*\n" ""
      package-upgrades)))
   nil
   (expand-file-name "misc/upgradable-packages.txt" config-root))
  (publish "misc/upgradable-packages.txt"))
