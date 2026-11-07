;;; config--pkg-marginalia.el --- Generated package (no.43) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: November 07, 2026
;; Modified: November 07, 2026
;; Version: 2026.11.07
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.43) from my config.
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

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let* ((attrs (file-attributes (substitute-in-file-name
                                        (marginalia--full-candidate cand))
                                       'integer)))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defconst +marginalia-file-size-pivot-bytes (* 10 1024 1024)
    "File size coloured orange, between the green and red extremes.")

  (defconst +marginalia-file-size-red-bytes (* 1024 1024 1024)
    "File size at which the size colour saturates to red.")

  (defun +marginalia-file-size-colorful (size)
    (let* ((orange (log +marginalia-file-size-pivot-bytes))
           (color (if (< size +marginalia-file-size-pivot-bytes)
                      (doom-blend 'orange 'green (/ (log (+ 1 size)) orange))
                    (doom-blend 'red 'orange
                                (min 1.0 (/ (- (log size) orange)
                                            (- (log +marginalia-file-size-red-bytes)
                                               orange)))))))
      (propertize (file-size-human-readable size)
                  'face (list :foreground color)))))

(provide 'config--pkg-marginalia)
;;; config--pkg-marginalia.el ends here
