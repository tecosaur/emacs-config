;;; config--pkg-ob-http.el --- Generated package (no.66) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.66) from my config.
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
;;   (package! ob-http :pin "b1428ea2a63bcb510e7382a1bf5fe82b19c104a7")
;;
;;; Code:


(use-package! ob-http
  :commands org-babel-execute:http)

(provide 'config--pkg-ob-http)
;;; config--pkg-ob-http.el ends here
