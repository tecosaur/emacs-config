;;; config--pkg-page-break-lines.el --- Generated package (no.46) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.46) from my config.
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
;;   (package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines")
;;     :pin "982571749c8fe2b5e2997dd043003a1b9fe87b38")
;;
;;; Code:


(use-package! page-break-lines
  :hook (prog-mode . page-break-lines-mode)
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (defvaralias 'page-break-lines-max-width 'fill-column)
  (defun +evil-forward-page ()
    "Call `forward-page', such that it works as intended with evil-mode."
    (interactive)
    (when (eq (char-after (point)) ?\^L)
      (forward-char 1))
    (forward-page))
  (defun +evil-backward-page ()
    "Call `backward-page', such that it works as intended with evil-mode."
    (interactive)
    (when (eq (char-after (point)) ?\^L)
      (backward-char 1))
    (backward-page))
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'+evil-backward-page
        :desc "Next page break" :nv "]" #'+evil-forward-page)
  (map! "<C-M-prior>" #'+evil-backward-page
        "<C-M-next>" #'+evil-forward-page))

(provide 'config--pkg-page-break-lines)
;;; config--pkg-page-break-lines.el ends here
