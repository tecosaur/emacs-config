;;; authinfo-mode.el -*- lexical-binding: t; -*-
(setq authinfo-colour-keywords
      '(("^#.*" . font-lock-comment-face)
        ("^\\(machine\\)[ \t]+\\([^ \t\n]+\\)"
         (1 font-lock-variable-name-face)
         (2 font-lock-builtin-face))
        ("\\(login\\)[ \t]+\\([^ \t\n]+\\)"
         (1 font-lock-comment-delimiter-face)
         (2 font-lock-keyword-face))
        ("\\(password\\)[ \t]+\\([^ \t\n]+\\)"
         (1 font-lock-comment-delimiter-face)
         (2 font-lock-doc-face))
        ("\\(port\\)[ \t]+\\([^ \t\n]+\\)"
         (1 font-lock-comment-delimiter-face)
         (2 font-lock-type-face))
        ("\\([^ \t\n]+\\)[, \t]+\\([^ \t\n]+\\)"
         (1 font-lock-constant-face)
         (2 nil))))

(defun authinfo-colour--hide-passwords (start end)
  "Just `authinfo--hide-passwords' with a different colour face overlay."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward "\\bpassword +\\([^\n\t ]+\\)"
                                nil t)
        (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put overlay 'display (propertize "****"
                                                    'face 'font-lock-doc-face))
          (overlay-put overlay 'reveal-toggle-invisible
                       #'authinfo-colour--toggle-display))))))

(defun authinfo-colour--toggle-display (overlay hide)
  "Just `authinfo--toggle-display' with a different colour face overlay."
  (if hide
      (overlay-put overlay 'display (propertize "****" 'face 'font-lock-doc-face))
    (overlay-put overlay 'display nil)))

(defvar authinfo-hide-passwords t
  "Whether to hide passwords in authinfo.")

(define-derived-mode authinfo-colour-mode fundamental-mode "Authinfo"
  "Major mode for editing .authinfo files.

Like `fundamental-mode', just with colour and passoword hiding."
  (font-lock-add-keywords nil authinfo-colour-keywords)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (when authinfo-hide-passwords
    (authinfo-colour--hide-passwords (point-min) (point-max))
    (reveal-mode)))

(provide 'authinfo-colour-mode)
