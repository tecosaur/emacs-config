;;; config-ox-md.el --- Generated package (no.89) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.89) from my config.
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

(require 'ox-md)

(defadvice! org-md-plain-text-unicode-a (orig-fn text info)
  "Locally rebind `org-html-special-string-regexps'"
  :around #'org-md-plain-text
  (let ((org-html-special-string-regexps
         '(("\\\\-" . "-")
           ("---\\([^-]\\|$\\)" . "—\\1")
           ("--\\([^-]\\|$\\)" . "–\\1")
           ("\\.\\.\\." . "…")
           ("<->" . "⟷")
           ("->" . "→")
           ("<-" . "←"))))
    (funcall orig-fn text (plist-put info :with-smart-quotes nil))))

(after! ox-md
  (defun org-md-latex-fragment (latex-fragment _contents info)
    "Transcode a LATEX-FRAGMENT object from Org to Markdown."
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "$" (substring frag 2 -2) "$"))
       ((string-match-p "^\\\\\\[" frag)
        (concat "$$" (substring frag 2 -2) "$$"))
       (t (message "unrecognised fragment: %s" frag)
          frag))))

  (defun org-md-latex-environment (latex-environment contents info)
    "Transcode a LATEX-ENVIRONMENT object from Org to Markdown."
    (concat "$$\n"
            (org-html-latex-environment latex-environment contents info)
            "$$\n"))

  (defun org-utf8-entity (entity _contents _info)
    "Transcode an ENTITY object from Org to utf-8.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
    (org-element-property :utf-8 entity))

  ;; We can't let this be immediately parsed and evaluated,
  ;; because eager macro-expansion tries to call as-of-yet
  ;; undefined functions.
  ;; NOTE in the near future this shouldn't be required
  (eval
   '(dolist (extra-transcoder
             '((latex-fragment . org-md-latex-fragment)
               (latex-environment . org-md-latex-environment)
               (entity . org-utf8-entity)))
      (unless (member extra-transcoder (org-export-backend-transcoders
                                        (org-export-get-backend 'md)))
        (push extra-transcoder (org-export-backend-transcoders
                                (org-export-get-backend 'md)))))))

(provide 'config-ox-md)
;;; config-ox-md.el ends here
