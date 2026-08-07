;;; config-doom.el --- Generated package (no.4) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: August 07, 2026
;; Modified: August 07, 2026
;; Version: 2026.08.07
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
		   (let (required-fonts available-fonts missing-fonts)
		     (setq required-fonts
			   '("JetBrains ?Mono.*" "Overpass"
			     "JuliaMono" "IBM Plex Mono"
			     "Merriweather" "Alegreya"
			     "Twitter Color Emoji"))
		     (setq available-fonts
			   (delete-dups
			    (or (font-family-list)
				(and (executable-find "fc-list")
				     (with-temp-buffer
				       (call-process "fc-list" nil t
						     nil ":" "family")
				       (split-string (buffer-string)
						     "[,\n]"))))))
		     (setq missing-fonts
			   (delq nil
				 (mapcar
				  (lambda (font)
				    (unless
					(delq nil
					      (mapcar
					       (lambda (f)
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
			       (lambda (font)
				 (propertize font 'face
					     'font-lock-variable-name-face))
			       '("JetBrains ?Mono.*" "Overpass"
				 "JuliaMono" "IBM Plex Mono"
				 "Merriweather" "Alegreya"
				 "Twitter Color Emoji")
			       ", ")))
		   (sleep-for 0.5)))))


(defvar doom-theme-light 'doom-tomorrow-day
  "The theme to use on light-mode systems.")
(defvar doom-theme-dark 'doom-vibrant
  "The theme to use on dark-mode systems.")

(setq doom-theme ; Set according to the env var or system-dependent default
      (let ((env-theme (getenv "DOOM_THEME")))
        (if env-theme
            (intern env-theme) ; Note: `intern-soft' doesn't work here
          doom-theme-dark)))

(require 'dbus)

(defun +doom-theme--from-system (system-theme)
  "Pick a doom theme based on the SYSTEM-THEME number."
  (pcase system-theme
    (1 doom-theme-dark) (2 doom-theme-light) (_ doom-theme-dark)))

(defun +doom-theme--dbus-update (namespace setting value)
  "Handle a D-Bus signal for system theme changes."
  (when
      (and (string= namespace "org.freedesktop.appearance")
	   (string= setting "color-scheme")
	   (not (memq (car-safe value) '(0 nil))))
    (let*
	((value (car value)) (theme (+doom-theme--from-system value))
	 (alt-value (if (eq value 1) 2 1))
	 (alt-theme (+doom-theme--from-system alt-value)))
      (when (memq alt-theme custom-enabled-themes)
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme theme t)))))

(when
    (and (display-graphic-p (selected-frame))
	 (not (getenv "DOOM_THEME")))
  (let
      ((system-theme
	(caar
	 (ignore-errors
	   (dbus-call-method :session "org.freedesktop.portal.Desktop"
			     "/org/freedesktop/portal/desktop"
			     "org.freedesktop.portal.Settings" "Read"
			     "org.freedesktop.appearance"
			     "color-scheme")))))
    (setq doom-theme (+doom-theme--from-system system-theme)))
  (dbus-register-signal :session "org.freedesktop.portal.Desktop"
			"/org/freedesktop/portal/desktop"
			"org.freedesktop.portal.Settings"
			"SettingChanged" #'+doom-theme--dbus-update))


(declare-function 'xterm--query "term/xterm")

(defvar term-background-rgb nil
  "A RGB triple corresponding to the current terminal background, if known.")

(defun +interpret-term-bg ()
  "Examine an OSC color query response, and set `term-background-rgb' accordingly."
  (let ((str "")
        chr)
    ;; The reply should be: \e ] 11 ; rgb: NUMBER1 / NUMBER2 / NUMBER3 \e \\
    (while (and (setq chr (xterm--read-event-for-query)) (not (equal chr ?\\)))
      (setq str (concat str (string chr))))
    (when (string-match
           "rgb:\\([a-f0-9]+\\)/\\([a-f0-9]+\\)/\\([a-f0-9]+\\)" str)
      (let ((r (string-to-number (match-string 1 str) 16))
            (g (string-to-number (match-string 2 str) 16))
            (b (string-to-number (match-string 3 str) 16)))
        (setq term-background-rgb (list r g b))))))

(defun +doom-init-theme-termaware ()
  "Update `doom-theme' if in a terminal, unless DOOM_THEME has been set."
  (let (term-shade)
    (when (and (not (display-graphic-p (selected-frame)))
               (not (getenv "DOOM_THEME"))
               (require 'term/xterm nil t))
      (message "Querying terminal background color")
      (xterm--query "\e]11;?\e\\" '(("\e]11;" . +interpret-term-bg)))
      (when term-background-rgb
        (setq term-shade (if (< (apply #'+ term-background-rgb) (* 0.6 3 65535))
                             'dark 'light))
        (pcase term-shade
          ('dark (setq doom-theme doom-theme-dark))
          ('light (setq doom-theme doom-theme-light)))))
    (doom-init-theme-h)
    (when term-shade
      (set-face-background (if (featurep 'solaire-mode) 'solaire-default-face 'default)
                           "unspecified-bg" (selected-frame)))))

(remove-hook 'window-setup-hook #'doom-init-theme-h)
(remove-hook 'after-init-hook   #'doom-init-theme-h)
(add-hook    'after-init-hook   #'+doom-init-theme-termaware 'append)

(delq! t custom-theme-load-path)

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
