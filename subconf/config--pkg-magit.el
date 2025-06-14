;;; config--pkg-magit.el --- Generated package (no.22) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: June 14, 2025
;; Modified: June 14, 2025
;; Version: 2025.06.14
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.22) from my config.
;;
;;  During generation, dependency on other aspects of my configuration and
;;  packages is inferred via (regexp-based) static analysis.  While this seems
;;  to do a good job, this method is imperfect.  This code likely depends on
;;  utilities provided by Doom, and if you try to run it in isolation you may
;;  discover the code makes more assumptions.
;;
;;  That said, I've found pretty good results so far.
;;
;;; Code:

(defvar +magit-project-commit-templates-alist nil
  "Alist of toplevel dirs and template hf strings/functions.")
(after! magit
  (defvar +magit-default-forge-remote "git@ssh.tecosaur.net:tec/%s.git"
  "Format string that fills out to a remote from the repo name.
Set to nil to disable this functionality.")
(defadvice! +magit-remote-add--streamline-forge-a (args)
  "Prompt to setup a remote using `+magit-default-forge-remote'."
  :filter-args #'magit-remote-add
  (interactive
   (let ((default-name
          (subst-char-in-string
           ?\s ?-
           (file-name-nondirectory
            (directory-file-name
             (or (doom-project-root) default-directory))))))
     (or (and +magit-default-forge-remote
              (not (magit-list-remotes))
              (eq (read-char-choice
                   (format "Setup %s remote? [y/n]: "
                           (replace-regexp-in-string
                            "\\`\\(?:[^@]+@\\|https://\\)\\([^:/]+\\)[:/].*\\'" "\\1"
                            +magit-default-forge-remote))
                   '(?y ?n))
                  ?y)
              (let ((name (read-string "Name: " default-name)))
                (list "origin" (format +magit-default-forge-remote name)
                      (transient-args 'magit-remote))))
         (let ((origin (magit-get "remote.origin.url"))
               (remote (magit-read-string-ns "Remote name"))
               (gh-user (magit-get "github.user")))
           (and (equal remote gh-user)
                (if origin
                    (and
                     (string-match "\\`https://github\\.com/\\([^/]+\\)/\\([^/]+\\)\\.git\\'"
                                   origin)
                     (not (string= (match-string 1 origin) gh-user)))
                  t)
                (setq origin
                      (if origin
                          (replace-regexp-in-string
                           "\\`https://github\\.com/" "git@github.com:"
                           origin)
                        (format "git@github.com:%s/%s" gh-user (read-string "GitHub repo Name: " default-name)))))
           (list remote
                 (magit-read-url
                  "Remote url"
                  (and origin
                       (string-match "\\([^:/]+\\)/[^/]+\\(\\.git\\)?\\'" origin)
                       (replace-match remote t t origin 1)))
                 (transient-args 'magit-remote))))))
  args)
(defun +magit-fill-in-commit-template ()
  "Insert template from `+magit-fill-in-commit-template' if applicable."
  (when-let ((template (and (save-excursion (goto-char (point-min)) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
                            (cdr (assoc (file-name-base (directory-file-name (magit-toplevel)))
                                        +magit-project-commit-templates-alist)))))
    (goto-char (point-min))
    (insert (if (stringp template) template (funcall template)))
    (goto-char (point-min))
    (end-of-line)))
(add-hook 'git-commit-setup-hook #'+magit-fill-in-commit-template 90)
(defun +org-commit-message-template ()
  "Create a skeleton for an Org commit message based on the staged diff."
  (let (change-data last-file file-changes temp-point)
    (with-temp-buffer
      (apply #'call-process magit-git-executable
             nil t nil
             (append
              magit-git-global-arguments
              (list "diff" "--cached")))
      (goto-char (point-min))
      (while (re-search-forward "^@@\\|^\\+\\+\\+ b/" nil t)
        (if (looking-back "^\\+\\+\\+ b/" (line-beginning-position))
            (progn
              (push (list last-file file-changes) change-data)
              (setq last-file (buffer-substring-no-properties (point) (line-end-position))
                    file-changes nil))
          (setq temp-point (line-beginning-position))
          (re-search-forward "^\\+\\|^-" nil t)
          (end-of-line)
          (cond
           ((string-match-p "\\.el$" last-file)
            (when (re-search-backward "^\\(?:[+-]? *\\|@@[ +-\\d,]+@@ \\)(\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\)" temp-point t)
              (re-search-forward "\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\) \\([^[:space:]\n]+\\)" nil t)
              (push (match-string 1) file-changes)))
           ((string-match-p "\\.org$" last-file)
            (when (re-search-backward "^[+-]\\*+ \\|^@@[ +-\\d,]+@@ \\*+ " temp-point t)
              (re-search-forward "@@ \\*+ " nil t)
              (push (buffer-substring-no-properties (point) (line-end-position)) file-changes)))))))
    (setq file-changes (delete-dups file-changes))
    (push (list last-file file-changes) change-data)
    (setq change-data (delete '(nil nil) change-data))
    (concat
     (if (= 1 (length change-data))
         (replace-regexp-in-string "^.*/\\|.[a-z]+$" "" (caar change-data))
       "?")
     ": \n\n"
     (mapconcat
      (lambda (file-changes)
        (if (cadr file-changes)
            (format "* %s (%s): "
                    (car file-changes)
                    (mapconcat #'identity (cadr file-changes) ", "))
          (format "* %s: " (car file-changes))))
      change-data
      "\n\n"))))

(add-to-list '+magit-project-commit-templates-alist (cons "org" #'+org-commit-message-template)))

(provide 'config--pkg-magit)
;;; config--pkg-magit.el ends here
