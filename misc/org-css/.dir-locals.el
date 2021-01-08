;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((scss-mode . ((after-save-hook . (lambda ()
                                    (shell-command-to-string "sassc main.scss main.css")
                                    (org-html-reload-fancy-style))))))
