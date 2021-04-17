;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((scss-mode . ((after-save-hook . (lambda ()
                                    (shell-command "./build.sh")
                                    (org-html-reload-fancy-style))))))
