;;; config-autocorrect.el --- Generated package (no.27) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.27) from my config.
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
;;   (package! autocorrect :recipe (:local-repo "lisp/autocorrect"))
;;
;;; Code:


(use-package! autocorrect
  :after jinx
  :config
  ;; Integrate with Jinx
  (defun autocorrect-jinx-record-correction (overlay corrected)
    "Record that Jinx corrected the text in OVERLAY to CORRECTED."
    (let ((text
           (buffer-substring-no-properties
            (overlay-start overlay)
            (overlay-end overlay))))
      (autocorrect-record-correction text corrected)))

  (defun autocorrect-jinx-check-spelling (word)
    "Check if WORD is valid."
    ;; Mostly a copy of `jinx--word-valid-p', just without the buffer substring.
    ;; It would have been nice if `jinx--word-valid-p' implemented like this
    ;; with `jinx--this-word-valid-p' (or similar) as the at-point variant.
    (or (member word jinx--session-words)
        ;; Allow capitalized words
        (and (string-match-p "\\`[[:upper:]][[:lower:]]+\\'" word)
             (cl-loop
              for w in jinx--session-words
              thereis (and (string-equal-ignore-case word w)
                           (string-match-p "\\`[[:lower:]]+\\'" w))))
        (cl-loop for dict in jinx--dicts
                 thereis (jinx--mod-check dict word))))

  (defun autocorrect-jinx-appropriate (pos)
    "Return non-nil if it is appropriate to spellcheck at POS according to jinx."
    (and (not (jinx--face-ignored-p pos))
         (not (jinx--regexp-ignored-p pos))))

  (setq autocorrect-check-spelling-function #'autocorrect-jinx-check-spelling)
  (add-to-list 'autocorrect-predicates #'autocorrect-jinx-appropriate)
  (advice-add 'jinx--correct-replace :before #'autocorrect-jinx-record-correction)

  ;; Run setup
  (run-with-idle-timer 0.5 nil #'autocorrect-setup)

  ;; Make work with evil-mode
  (evil-collection-set-readonly-bindings 'autocorrect-list-mode-map)
  (evil-collection-define-key 'normal 'autocorrect-list-mode-map
    (kbd "a") #'autocorrect-create-correction
    (kbd "x") #'autocorrect-remove-correction
    (kbd "i") #'autocorrect-ignore-word))

(provide 'config-autocorrect)
;;; config-autocorrect.el ends here
