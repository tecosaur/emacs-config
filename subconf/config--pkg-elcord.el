;;; config--pkg-elcord.el --- Generated package (no.54) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: May 16, 2025
;; Modified: May 16, 2025
;; Version: 2025.05.16
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.54) from my config.
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
;;   (package! elcord :pin "e2775f40ec55dfdceea83d535dff77d60534b6bc")
;;
;;; Code:


(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(provide 'config--pkg-elcord)
;;; config--pkg-elcord.el ends here
