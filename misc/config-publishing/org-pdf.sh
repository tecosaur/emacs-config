#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (expand-file-name (format "%s-log.txt" (file-name-base load-file-name))))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise 'full)

;;; Actually do the exporting now

(message "[34] Opening config file: %s"
         (expand-file-name "config.org" config-root))

(require 'vc) ; need this for modification-time macro
(require 'org)
(require 'org-persist)
(remove-hook 'kill-emacs-hook #'org-persist-gc)
(require 'ox-latex)

(advice-add 'pdf-tools-install :around #'ignore)
(advice-add 'pdf-info-features :around #'ignore)

;; When called from `org-latex-src--engrave-code' the function
;; `TeX-run-style-hooks' ends up calling one of a few reftex functions tht
;; themselves call `reftex-TeX-master-file' which then invokes
;; `(expand-file-name nil)', causing an error.
(require 'tex)
(advice-add 'TeX-run-style-hooks :around #'ignore)

(setq org-link-parameters
      (delq (assoc "pdf" org-link-parameters)
            org-link-parameters))

;; For some faces
(require 'highlight-numbers)
(require 'highlight-quoted)
(require 'rainbow-delimiters)

(require 'engrave-faces-latex)
(load (expand-file-name "doom-one-light-engraved-theme.el"))
(engrave-faces-use-theme 'doom-one-light)

(with-temp-buffer
  (let ((default-directory config-root)
        (buffer-file-name (expand-file-name "config.org" config-root))
        (org-export-coding-system 'utf-8)
        (org-export-with-broken-links t)
        (org-resource-download-policy t)
        (org-persist-disable-when-emacs-Q nil)
        org-mode-hook org-load-hook)
    (insert-file-contents (expand-file-name "config.org" config-root))
    (goto-char (point-max))
    (insert (shell-command-to-string (expand-file-name "./gen-org-snippets.sh" script-root)))
    (message "[33] Exporting %s" (buffer-file-name))
    (org-mode)
    ;; There isn't actually any Julia code in config.org
    (setq org-export-conditional-features
          (delq (rassq 'julia-code org-export-conditional-features)
                org-export-conditional-features))
    (org-latex-export-to-pdf)))

(publish "config.pdf")

(message "[1;32] Config export complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
