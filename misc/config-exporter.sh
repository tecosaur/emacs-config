#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq start-time (float-time)
      exit-code 0)

(defvar files-to-htmlize '("init.el" "config.el" "packages.el"))
(defvar htmlize-theme 'doom-one-light)

(defvar file-root (file-name-directory ; parent dir
                   (directory-file-name
                    (file-name-directory load-file-name))))

;;; Report errors

(setq debug-on-error t
      doom-debug-p t)

(setq log-file (expand-file-name "exporter-log.txt" (file-name-directory load-file-name)))

;;; If running just to to htmlization (i.e., in a terminal)

(unless noninteractive

  (write-region "" nil log-file)

  (defun logged-message (orig-fn &rest args)
    (unless inhibit-message
      (let ((inhibit-message t)
            (msg (apply orig-fn args)))
        (princ msg #'external-debugging-output)
        (append-to-file msg nil log-file)
        (append-to-file "\n" nil log-file))))
  (advice-add 'message :around #'logged-message)

  (advice-add 'theme-magic-from-emacs :override #'ignore)
  (advice-add 'format-all-buffer :override #'ignore)

  (load (expand-file-name "~/.emacs.d/init.el"))

  (require 'htmlize)

  (defun my-htmlize-file (file)
    (message "Htmlizing %s" file)
    (let ((output-file (expand-file-name
                        (htmlize-make-file-name (file-name-nondirectory file))
                        (file-name-directory file))))
      (with-current-buffer (find-file-literally file)
        (normal-mode)
        (font-lock-ensure)
        (set-buffer-modified-p nil)
        (with-current-buffer (htmlize-buffer-1)
          (goto-char (point-min))
          (replace-string "</title>\n"
                          "</title>
  <style>
    body > pre {
      font-size: 1rem;
      max-width: min(100rem, 100%);
      width: max-content;
      white-space: pre-wrap;
      margin: auto;
    }
  </style>\n")
          (let ((inhibit-message t))
            (write-file output-file)
            (kill-buffer (current-buffer))))
        (kill-buffer (current-buffer)))))

  (load-theme htmlize-theme t)

  (dolist (file files-to-htmlize)
    (my-htmlize-file (expand-file-name file file-root)))

  (kill-emacs exit-code))

;;; Running 'normally' (batch mode) from now on

;;; Colourful messaging and errors

(defun timed-coloured-message (orig-fn format-str &rest args)
  (cond
   ((eq (car args) 'unmodified)
    (apply orig-fn format-str (cdr args)))
   ((or (not format-str) (string-match-p "\\[%\\.1fs\\]" format-str))
    (apply orig-fn format-str args))
   (t
    (apply orig-fn
           (concat (if (string-match-p "^\\[[0-9;]+\\]" format-str)
                       (replace-regexp-in-string
                        "^\\(?:\\[\\([0-9;]+\\)\\] ?\\)?"
                        "\033[\\1m[%.1fs] "
                        format-str)
                     (concat "[%.1fs] " format-str))
                   "\033[0;90m")
           (append (list (- (float-time) start-time))
                   args)))))
(advice-add 'message :around #'timed-coloured-message)

(defun red-error (orig-fn &rest args)
  (message "\033[0;31m" 'unmodified)
  (apply orig-fn args)
  (message "\033[0m" 'unmodified)
  (setq exit-code 1))
(advice-add 'debug :around #'red-error)

;;; Htmlization

(message "[0;1] Starting htmlization process for %s" files-to-htmlize)

(setq htmlization-process
      (start-process "htmlize" nil
                     "script" "-eqfc"
                     (format "TERM=xterm-direct emacs --quick -nw --eval '(load (expand-file-name \"%s\"))'" load-file-name)))

(set-process-sentinel
 htmlization-process
 (lambda (process _signal)
   (when (eq (process-status process) 'exit)
     (if (= 0 (process-exit-status process))
         (message "[1;35] Htmlization finished sucessfully")
       (message "[31] Htmlization process failed!")
       (message "\033[0;31m      %s\033[0m"
                'unmodified
                (with-temp-buffer
                  (insert-file-contents-literally log-file)
                  (buffer-substring-no-properties (point-min) (point-max))))))))

(message "[0;1] Building config file export")

;;; Setup

(advice-add 'theme-magic-from-emacs :override #'ignore)
(advice-add 'format-all-buffer :override #'ignore)

(load (expand-file-name "~/.emacs.d/init.el"))

(when (and (featurep 'undo-tree) global-undo-tree-mode)
  (global-undo-tree-mode -1)
  (advice-add 'undo-tree-save-history :override #'ignore))

;;; Actually do the exporting now

(message "[34] Opening config file: %s"
         (expand-file-name "config.org" file-root))

(require 'vc) ; need this for modification-time macro

(with-current-buffer (find-file (expand-file-name "config.org" file-root))
  (message "[33] Exporting %s" (buffer-file-name))
  (org-html-export-to-html))

(message "[1;32] Config export complete!")

(while (not (eq (process-status htmlization-process) 'exit))
  (message "Waiting for Htmlization process to finish")
  (sleep-for 1))

(setq inhibit-message t)
(kill-emacs exit-code)
