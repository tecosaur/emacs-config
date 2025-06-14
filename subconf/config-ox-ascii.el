;;; config-ox-ascii.el --- Generated package (no.88) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.88) from my config.
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

(require 'ox-ascii)

(setq org-ascii-charset 'utf-8)

(when (executable-find "latex2text")
  (after! ox-ascii
    (defvar org-ascii-convert-latex t
      "Use latex2text to convert LaTeX elements to unicode.")

    (defadvice! org-ascii-latex-environment-unicode-a (latex-environment _contents info)
      "Transcode a LATEX-ENVIRONMENT element from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-environment
      (when (plist-get info :with-latex)
        (org-ascii--justify-element
         (org-remove-indentation
          (let* ((latex (org-element-property :value latex-environment))
                 (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                               org-ascii-convert-latex
                               (doom-call-process "latex2text" "-q" "--code" latex))))
            (if (= (car unicode) 0) ; utf-8 set, and sucessfully ran latex2text
                (cdr unicode) latex)))
         latex-environment info)))

    (defadvice! org-ascii-latex-fragment-unicode-a (latex-fragment _contents info)
      "Transcode a LATEX-FRAGMENT object from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-fragment
      (when (plist-get info :with-latex)
        (let* ((latex (org-element-property :value latex-fragment))
               (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                             org-ascii-convert-latex
                             (doom-call-process "latex2text" "-q" "--code" latex))))
          (if (and unicode (= (car unicode) 0)) ; utf-8 set, and sucessfully ran latex2text
              (cdr unicode) latex))))))

(provide 'config-ox-ascii)
;;; config-ox-ascii.el ends here
