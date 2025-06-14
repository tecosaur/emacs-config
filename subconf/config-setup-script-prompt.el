;;; config-setup-script-prompt.el --- Generated package (no.11) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.11) from my config.
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

(unless noninteractive
  (defun +config-run-setup nil
    (when-let
	((setup-file (expand-file-name "setup.sh" doom-user-dir))
	 ((file-exists-p setup-file))
	 (setup-content
	  (string-trim
	   (with-temp-buffer
	     (insert-file-contents setup-file) (buffer-string))
	   "#!/usr/bin/env bash"))
	 ((not (string-empty-p setup-content)))
	 ((yes-or-no-p
	   (format
	    "%s The setup script has content. Check and run the script?"
	    (propertize "Warning!" 'face '(bold warning))))))
      (find-file setup-file)
      (when (yes-or-no-p "Would you like to run this script?")
	(async-shell-command "./setup.sh"))))
  (add-hook! 'doom-init-ui-hook
    (run-at-time nil nil #'+config-run-setup)))


(provide 'config-setup-script-prompt)
;;; config-setup-script-prompt.el ends here
