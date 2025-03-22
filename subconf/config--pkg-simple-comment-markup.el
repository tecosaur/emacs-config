;;; config--pkg-simple-comment-markup.el --- Generated package (no.37) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 22, 2025
;; Modified: March 22, 2025
;; Version: 2025.03.22
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.37) from my config.
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
;;   (package! simple-comment-markup :recipe (:local-repo "lisp/simple-comment-markup"))
;;
;;; Code:


(use-package! simple-comment-markup
  :hook (prog-mode . simple-comment-markup-mode)
  :config
  (setq simple-comment-markup-set '(org markdown-code)))

(provide 'config--pkg-simple-comment-markup)
;;; config--pkg-simple-comment-markup.el ends here
