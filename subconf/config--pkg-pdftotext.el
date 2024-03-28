;;; config--pkg-pdftotext.el --- Generated package (no.94) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.94) from my config.
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
;;   (package! pdftotext :recipe (:local-repo "lisp/pdftotext"))
;;
;;; Code:


(use-package! pdftotext
  :init
  (unless (display-graphic-p)
    (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdftotext-mode))
    (add-to-list 'magic-mode-alist '("%PDF" . pdftotext-mode)))
  :config
  (unless (display-graphic-p) (after! pdf-tools (pdftotext-install)))
  ;; For prettyness
  (add-hook 'pdftotext-mode-hook #'spell-fu-mode-disable)
  (add-hook 'pdftotext-mode-hook (lambda () (page-break-lines-mode 1)))
  ;; I have no idea why this is needed
  (map! :map pdftotext-mode-map
        "<mouse-4>" (cmd! (scroll-down mouse-wheel-scroll-amount-horizontal))
        "<mouse-5>" (cmd! (scroll-up mouse-wheel-scroll-amount-horizontal))))


(provide 'config--pkg-pdftotext)
;;; config--pkg-pdftotext.el ends here
