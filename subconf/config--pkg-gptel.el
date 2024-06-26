;;; config--pkg-gptel.el --- Generated package (no.19) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: June 26, 2024
;; Modified: June 26, 2024
;; Version: 2024.06.26
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.19) from my config.
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
;;   (package! gptel :pin "9eea4be5ed9c7a651619347f6b3191d083ec252e")
;;
;;; Code:


(use-package! gptel
  :commands gptel gptel-menu gptel-mode gptel-send gptel-set-tpic
  :config
  (let (ollama-models)
    (when (executable-find "ollama")
      (with-temp-buffer
        (call-process "ollama" nil t nil "list")
        (goto-char (point-min))
        (forward-line 1)
        (while (and (not (eobp)) (looking-at "[^ \t]+"))
          (push (match-string 0) ollama-models)
          (forward-line 1))))
    (setq-default gptel-model "nous-hermes2:latest"
                  gptel-backend (gptel-make-ollama "Ollama" :models ollama-models :stream t)))
  (setq gptel-default-mode #'org-mode))

(provide 'config--pkg-gptel)
;;; config--pkg-gptel.el ends here
