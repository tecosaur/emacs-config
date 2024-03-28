;;; config-flycheck-org-lint.el --- Generated package (no.78) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 28, 2024
;; Modified: March 28, 2024
;; Version: 2024.03.28
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.78) from my config.
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

(defconst flycheck-org-lint-form
  (flycheck-prepare-emacs-lisp-form
    (require 'org)
    (require 'org-lint)
    (require 'org-attach)
    (let ((source (car command-line-args-left))
          (process-default-directory default-directory))
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        (setq default-directory process-default-directory)
        (delay-mode-hooks (org-mode))
        (setq delayed-mode-hooks nil)
        (dolist (err (org-lint))
          (let ((inf (cl-second err)))
            (princ (elt inf 0))
            (princ ": ")
            (princ (elt inf 2))
            (terpri)))))))

(defconst flycheck-org-lint-variables
  '(org-directory
    org-id-locations
    org-id-locations-file
    org-attach-id-dir
    org-attach-use-inheritance
    org-attach-id-to-path-function-list
    org-link-parameters)
  "Variables inherited by the org-lint subprocess.")

(defun flycheck-org-lint-variables-form ()
  (require 'org-attach)  ; Needed to make variables available
  `(progn
     ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                (seq-filter #'boundp flycheck-org-lint-variables))))

(eval ; To preveant eager macro expansion form loading flycheck early.
 '(flycheck-define-checker org-lint
   "Org buffer checker using `org-lint'."
   :command ("emacs" (eval flycheck-emacs-args)
             "--eval" (eval (concat "(add-to-list 'load-path \""
                                    (file-name-directory (locate-library "org"))
                                    "\")"))
             "--eval" (eval (flycheck-sexp-to-string
                             (flycheck-org-lint-variables-form)))
             "--eval" (eval (flycheck-sexp-to-string
                             (flycheck-org-lint-customisations-form)))
             "--eval" (eval flycheck-org-lint-form)
             "--" source)
   :error-patterns
   ((error line-start line ": " (message) line-end))
   :modes org-mode))

(add-to-list 'flycheck-checkers 'org-lint)

(defun flycheck-org-lint-customisations-form ()
  `(progn
     (require 'ox)
     (cl-pushnew '(:latex-cover-page nil "coverpage" nil)
                 (org-export-backend-options (org-export-get-backend 'latex)))
     (cl-pushnew '(:latex-font-set nil "fontset" nil)
                 (org-export-backend-options (org-export-get-backend 'latex)))))

(provide 'config-flycheck-org-lint)
;;; config-flycheck-org-lint.el ends here
