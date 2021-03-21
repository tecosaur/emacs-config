#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'subr-x)

(defvar config-root (file-name-directory ; $DOOM_DIR/
                     (directory-file-name
                      (file-name-directory ; $DOOM_DIR/misc
                       (directory-file-name
                        (file-name-directory load-file-name))))))

(setq snippets (concat config-root "snippets/"))

(defun rename (name)
  (let ((out
         (string-trim
          (replace-regexp-in-string
           "[-_]" " "
           name))))
    (if (string= "" out) "File template"
      (let ((sentence (expand-text-abbrevs out)))
        (concat (char-to-string (upcase (aref sentence 0)))
                (substring sentence 1))))))

(defun expand-text-abbrevs (str)
  (mapconcat
   (lambda (word)
     (cond
      ((string= word "src") "source")
      ((string-match-p "args?" word) (replace-regexp-in-string "arg" "argument" word))
      (t word)))
   (split-string str)
   " "))

(princ
 (concat
  "\n\n** Snippets\n"
  (mapconcat
   (lambda (s-mode)
     (concat
      "*** " (rename s-mode) "\n"
      (mapconcat
       (lambda (s-file)
         (concat
          (rename (file-name-nondirectory s-file))
          "\n#+begin_src snippet\n"
          (with-temp-buffer
            (insert-file-contents s-file)
            (goto-char (point-min))
            (while (re-search-forward "^#\\+" nil t)
              (replace-match ",#+"))
            (buffer-string))
          "\n#+end_src"))
       (directory-files (concat snippets s-mode) t "[^.]$")
       "\n\n")))
   (directory-files snippets nil "^[^.]+$")
   "\n\n")))
