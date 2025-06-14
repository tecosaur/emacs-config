;;; config--pkg-visual-fill-column.el --- Generated package (no.49) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.49) from my config.
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

(require 'visual-fill-column)

(defun +visual-fill-column--window-max-text-width--fixed (&optional window)
  "Return the maximum possible text width of WINDOW.
The maximum possible text width is the width of the current text
area plus the margins, but excluding the fringes, scroll bar, and
right divider.  WINDOW defaults to the selected window.  The
return value is scaled to account for `text-scale-mode-amount'
and `text-scale-mode-step'."
  (or window (setq window (selected-window)))
  (let* ((margins (window-margins window))
         (buffer (window-buffer window))
         (scale (if (and visual-fill-column-adjust-for-text-scale
                         (boundp 'text-scale-mode-step)
                         (boundp 'text-scale-mode-amount))
                    (with-current-buffer buffer
                      (expt text-scale-mode-step
                            text-scale-mode-amount))
                  1.0))
         (remap-scale
          (if (>= emacs-major-version 29)
              (/ (window-width window 'remap) (float (window-width window)))
            1.0)))
    (truncate (/ (+ (window-width window (and (>= emacs-major-version 29) 'remap))
                    (* (or (car margins) 0) remap-scale)
                    (* (or (cdr margins) 0) remap-scale))
                 (float scale)))))

(advice-add 'visual-fill-column--window-max-text-width
            :override #'+visual-fill-column--window-max-text-width--fixed)

(defun +visual-fill-column--set-margins--fixed (window)
  "Set window margins for WINDOW."
  ;; Calculate left & right margins.
  (let* ((total-width (visual-fill-column--window-max-text-width window))
         (remap-scale
          (if (>= emacs-major-version 29)
              (/ (window-width window 'remap) (float (window-width window)))
            1.0))
         (width (or visual-fill-column-width
                    fill-column))
         (margins (if (< (- total-width width) 0) ; margins must be >= 0
                      0
                    (round (/ (- total-width width) remap-scale))))
         (left (if visual-fill-column-center-text
                   (/ margins 2)
                 0))
         (right (- margins left)))

    (if visual-fill-column-extra-text-width
        (let ((add-width (visual-fill-column--add-extra-width left right visual-fill-column-extra-text-width)))
          (setq left (car add-width)
                right (cdr add-width))))

    ;; put an explicitly R2L buffer on the right side of the window
    (when (and (eq bidi-paragraph-direction 'right-to-left)
               (= left 0))
      (setq left right)
      (setq right 0))

    (set-window-margins window left right)))

(advice-add 'visual-fill-column--set-margins
            :override #'+visual-fill-column--set-margins--fixed)

(provide 'config--pkg-visual-fill-column)
;;; config--pkg-visual-fill-column.el ends here
