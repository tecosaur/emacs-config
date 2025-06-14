;;; config-flycheck-org-lint.el --- Generated package (no.79) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.79) from my config.
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

(defconst flycheck-org-lint-form
  (flycheck-prepare-emacs-lisp-form
    (require 'org)
    (require 'org-lint)
    (require 'org-attach)
    (let ((source (car command-line-args-left))
          (process-default-directory default-directory))
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        (setq default-directory process-default-directory)
        (delay-mode-hooks (org-mode))
        (setq delayed-mode-hooks nil)
        (dolist (err (org-lint))
          (let ((inf (cl-second err)))
            (princ (elt inf 0))
            (princ ": ")
            (princ (elt inf 2))
            (terpri)))))))

(defconst flycheck-org-lint-variables
  '(org-directory
    org-id-locations
    org-id-locations-file
    org-attach-id-dir
    org-attach-use-inheritance
    org-attach-id-to-path-function-list
    org-link-parameters)
  "Variables inherited by the org-lint subprocess.")

(defconst flycheck-org-lint-babel-langs
  '(C C++ D LaTeX R TeX antlr ash asm asymptote autoconf awk bash bash-ts bash2 bat beamer c c++ c++-ts c-ts c-ts-base calc cfengine2 cfengine3 change-log clojure clojurescript cmake-ts common-lisp conf conf-colon conf-desktop conf-javaprop conf-ppd conf-space conf-toml conf-unix conf-windows conf-xdefaults cperl cpp csh csharp csharp-ts css css-base css-ts dash dcl delphi desktop ditaa dns doc-view--text-view dockerfile-ts doctex dot dsssl dtksh editorconfig-conf elisp elisp-byte-code elixir-ts emacs-lisp emacs-news emacs-news-view erts es eshell f90 fish forth fortran gdb-script gnuplot gnus-article-edit gnus-score go-mod-ts go-ts groovy haskell heex-ts help-fns--edit-value html html-ts icon idl idlwave indented-text itcsh java java-ts javascript jcsh js js-base js-json js-jsx js-ts jsh json-ts julia ksh ksh88 latex ld-script less-css lilypond lisp lisp-data lisp-interaction lua lua-ts m2 m4 mail makefile makefile-automake makefile-bsdmake makefile-gmake makefile-imake makefile-makepp matlab maxima mercury message meta-common metafont metapost mh-letter mh-show mhtml mixal mksh modula-2 nroff nxml oash objc ocaml octave opascal org outline paragraph-indent-text pascal pdksh perl php-ts pike plain-TeX plain-tex plantuml plstore posh posix processing prog prolog ps python python-base python-ts rc rpm rst ruby ruby-base ruby-ts rust-ts sass scheme screen scss sed sgml sh sh-base shell shell-script sieve simula slitex sql sqlite tcl tcsh tex texinfo text todo-edit toml toml-ts tsx-ts typescript-ts typescript-ts-base vera verilog vhdl wksh wsh xml yaml-ts zone zsh)
  "Languages that org-babel should know of.")

(defun flycheck-org-lint-variables-form ()
  (require 'org-attach)  ; Needed to make variables available
  `(progn
     ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                (seq-filter #'boundp flycheck-org-lint-variables))))

(defun flycheck-org-lint-babel-langs-form ()
  `(progn
     ,@(mapcar
        (lambda (lang)
          `(defun ,(intern (format "org-babel-execute:%s" lang)) (_body _params)
             "Stub for org-lint."))
        flycheck-org-lint-babel-langs)))

(eval ; To preveant eager macro expansion form loading flycheck early.
 '(flycheck-define-checker org-lint
   "Org buffer checker using `org-lint'."
   :command ("emacs" (eval flycheck-emacs-args)
             "--eval" (eval (concat "(add-to-list 'load-path \""
                                    (file-name-directory (locate-library "org"))
                                    "\")"))
             "--eval" (eval (flycheck-sexp-to-string
                             (flycheck-org-lint-variables-form)))
             "--eval" (eval (flycheck-sexp-to-string
                             (flycheck-org-lint-customisations-form)))
             "--eval" (eval (flycheck-sexp-to-string
                             (flycheck-org-lint-babel-langs-form)))
             "--eval" (eval flycheck-org-lint-form)
             "--" source)
   :error-patterns
   ((error line-start line ": " (message) line-end))
   :modes org-mode))

(add-to-list 'flycheck-checkers 'org-lint)

(defun flycheck-org-lint-customisations-form ()
  `(progn
     (require 'ox)
     (cl-pushnew '(:latex-cover-page nil "coverpage" nil)
                 (org-export-backend-options (org-export-get-backend 'latex)))
     (cl-pushnew '(:latex-font-set nil "fontset" nil)
                 (org-export-backend-options (org-export-get-backend 'latex)))))

(provide 'config-flycheck-org-lint)
;;; config-flycheck-org-lint.el ends here
