;;; config--pkg-org-glossary.el --- Generated package (no.71) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.71) from my config.
;;
;;  During generation, dependency on other aspects of my configuration and
;;  packages is inferred via (regexp-based) static analysis.  While this seems
;;  to do a good job, this method is imperfect.  This code likely depends on
;;  utilities provided by Doom, and if you try to run it in isolation you may
;;  discover the code makes more assumptions.
;;
;;  That said, I've found pretty good results so far.
;;
;;  Package statement:
;;   (package! org-glossary :recipe (:local-repo "lisp/org-glossary"))
;;
;;; Code:


(use-package! org-glossary
  :hook (org-mode . org-glossary-mode)
  :config
  (setq org-glossary-collection-root "~/.config/doom/misc/glossaries/")
  (defun +org-glossary--latex-cdef (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
    (org-glossary--export-template
     (if (plist-get term-entry :uses)
         "*%d*\\emsp{}%v\\ensp{}@@latex:\\labelcpageref{@@%b@@latex:}@@\n"
       "*%d*\\emsp{}%v\n")
     backend info term-entry ref-index
     plural-p capitalized-p extra-parameters))
  (org-glossary-set-export-spec
   'latex t
   :backref "gls-%K-use-%r"
   :backref-seperator ","
   :definition-structure #'+org-glossary--latex-cdef))

(provide 'config--pkg-org-glossary)
;;; config--pkg-org-glossary.el ends here
