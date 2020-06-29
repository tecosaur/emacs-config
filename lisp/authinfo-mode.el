;; [[file:../config.org::*Authinfo][Authinfo:1]]
(setq authinfo-keywords
      '(("^#.*" . font-lock-comment-face)
        ("^\\(machine\\) \\([^ \t\n]+\\)"
         (1 font-lock-variable-name-face)
         (2 font-lock-builtin-face))
        ("\\(login\\) \\([^ \t\n]+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-string-face))
        ("\\(password\\) \\([^ \t\n]+\\)"
         (1 font-lock-constant-face)
         (2 font-lock-doc-face))
        ("\\(port\\) \\([^ \t\n]+\\)"
         (1 font-lock-type-face)
         (2 font-lock-type-face))))

(define-derived-mode authinfo-mode fundamental-mode "authinfo"
  "Major mode for editing the authinfo file."
  (font-lock-add-keywords nil authinfo-keywords)
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(provide 'authinfo-mode)
;; Authinfo:1 ends here
