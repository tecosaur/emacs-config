;;; config--pkg-doom-modeline.el --- Generated package (no.38) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.38) from my config.
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

(require 'doom-modeline)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(setq doom-modeline-height 45)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq doom-modeline-time-clock-size 0.65)

(use-package! doom-modeline-media-player
  :defer t
  :init
  (after! doom-modeline
    (add-to-list 'doom-modeline-fn-alist
                 (cons 'media-player #'doom-modeline-segment--media-player)))
  :config
  (defun +single-fullscreen-window-p ()
    (and (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
         (not (consp (car (window-tree))))))
  (setq doom-modeline-media-player #'+single-fullscreen-window-p
        doom-modeline-media-player-playback-indication 'dim))

(doom-modeline-def-segment buffer-name
  "Display the current buffer's name, without any other information."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-name)))

(doom-modeline-def-segment pdf-icon
  "PDF icon from nerd-icons."
  (concat
   (doom-modeline-icon sucicon "nf-seti-pdf" nil nil
   (doom-modeline-spc)
                       :face (if (doom-modeline--active)
                                 'nerd-icons-red
                               'mode-line-inactive)
                       :v-adjust 0.02)))

(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
              (total-page-str (number-to-string (pdf-cache-number-of-pages))))
          (concat
           (propertize
            (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                    " P" current-page-str)
            'face 'mode-line)
           (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

(doom-modeline-def-segment pdf-pages
  "Display PDF pages."
  (if (doom-modeline--active) doom-modeline--pdf-pages
    (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

(doom-modeline-def-modeline 'pdf
  '(bar window-number pdf-pages pdf-icon buffer-name)
  '(media-player misc-info matches major-mode process vcs))

(provide 'config--pkg-doom-modeline)
;;; config--pkg-doom-modeline.el ends here
