;;; config-doom.el --- Generated package (no.4) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.4) from my config.
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

(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
      doom-big-font (font-spec :family "JetBrains Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 26)
      doom-symbol-font (font-spec :family "JuliaMono")
      doom-emoji-font (font-spec :family "Twitter Color Emoji") ; Just used by me
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light))

(dolist (char '(?⏩ ?⏪ ?❓))
  (set-char-table-range char-script-table char 'symbol))

(add-hook! 'after-setting-font-hook
  (defun +emoji-set-font ()
    (set-fontset-font t 'emoji doom-emoji-font nil 'prepend)))

(defvar +emoji-rx
  (let (emojis)
    (map-char-table
     (lambda (char set)
       (when (eq set 'emoji)
         (push (copy-tree char) emojis)))
     char-script-table)
    (rx-to-string `(any ,@emojis)))
  "A regexp to find all emoji-script characters.")

(setq emoji-alternate-names
      '(("🙂" ":)")
        ("😄" ":D")
        ("😉" ";)")
        ("🙁" ":(")
        ("😆" "laughing face" "xD")
        ("🤣" "ROFL face")
        ("😢" ":'(")
        ("🥲" ":')")
        ("😮" ":o")
        ("😑" ":|")
        ("😎" "cool face")
        ("🤪" "goofy face")
        ("🤥" "pinnochio face" "liar face")
        ("😠" ">:(")
        ("😡" "angry+ face")
        ("🤬" "swearing face")
        ("🤢" "sick face")
        ("😈" "smiling imp")
        ("👿" "frowning imp")
        ("❤️" "<3")
        ("🫡" "o7")
        ("👍" "+1")
        ("👎" "-1")
        ("👈" "left")
        ("👉" "right")
        ("👆" "up")
        ("💯" "100")
        ("💸" "flying money")))

(when (>= emacs-major-version 29)
  (map! :leader
        (:prefix ("e" . "Emoji")
         :desc "Search" "s" #'emoji-search
         :desc "Recent" "r" #'emoji-recent
         :desc "List" "l" #'emoji-list
         :desc "Describe" "d" #'emoji-describe
         :desc "Insert" "i" #'emoji-insert
         :desc "Insert" "e" #'emoji-insert)))

(unless noninteractive
  (add-hook! 'doom-init-ui-hook
    (run-at-time nil nil
		 (lambda nil
		   (let
		       (required-fonts available-fonts missing-fonts)
		     (setq required-fonts
			   '("JetBrains ?Mono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya" "Twitter Color Emoji"))
		     (setq available-fonts
			   (delete-dups
			    (or
			     (font-family-list)
			     (and
			      (executable-find "fc-list")
			      (with-temp-buffer
				(call-process "fc-list" nil t nil ":" "family")
				(split-string
				 (buffer-string)
				 "[,\n]"))))))
		     (setq missing-fonts
			   (delq nil
				 (mapcar
				  (lambda
				    (font)
				    (unless
					(delq nil
					      (mapcar
					       (lambda
						 (f)
						 (string-match-p
						  (format "^%s$" font)
						  f))
					       available-fonts))
				      font))
				  required-fonts)))
		     (message "%s missing the following fonts: %s"
			      (propertize "Warning!" 'face
					  '(bold warning))
			      (mapconcat
			       (lambda
				 (font)
				 (propertize font 'face 'font-lock-variable-name-face))
			       '("JetBrains ?Mono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya" "Twitter Color Emoji")
			       ", ")))
		   (sleep-for 0.5)))))


(setq doom-theme
      (if (getenv "DOOM_THEME")
          (intern (getenv "DOOM_THEME"))
        'doom-vibrant))

(delq! t custom-theme-load-path)

(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)

(setq display-line-numbers-type 'relative)

(evil-define-command +evil-buffer-org-new (_count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)
        (setq-local doom-real-buffer-p t)))))

(map! :leader
      (:prefix "b"
       :desc "New empty Org buffer" "o" #'+evil-buffer-org-new))

(provide 'config-doom)
;;; config-doom.el ends here
