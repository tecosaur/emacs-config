;;; config-ox-beamer.el --- Generated package (no.86) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.86) from my config.
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

(require 'ox-beamer)

(setq org-beamer-theme "[progressbar=foot]metropolis")

(defun org-beamer-p (info)
  (eq 'beamer (and (plist-get info :back-end)
                   (org-export-backend-name (plist-get info :back-end)))))

(org-export-update-features 'beamer
  (beamer-setup
   :condition t
   :requires .missing-koma
   :prevents (italic-quotes condensed-lists cover-page)))

(org-export-update-features 'latex
  (.missing-koma
   :snippet "\\usepackage{scrextend}"
   :order 2))

(defvar org-beamer-metropolis-tweaks
  "\\NewCommandCopy{\\moldusetheme}{\\usetheme}
\\renewcommand*{\\usetheme}[2][]{\\moldusetheme[#1]{#2}
  \\setbeamertemplate{items}{$\\bullet$}
  \\setbeamerfont{block title}{size=\\normalsize, series=\\bfseries\\parbox{0pt}{\\rule{0pt}{4ex}}}}

\\makeatletter
\\newcommand{\\setmetropolislinewidth}{
  \\setlength{\\metropolis@progressinheadfoot@linewidth}{1.2px}}
\\makeatother

\\usepackage{etoolbox}
\\AtEndPreamble{\\setmetropolislinewidth}"
  "LaTeX preamble snippet that tweaks the Beamer metropolis theme styling.")

(org-export-update-features 'beamer
  (beamer-metropolis
   :condition (string-match-p "metropolis$" (plist-get info :beamer-theme))
   :snippet org-beamer-metropolis-tweaks
   :order 3))

(setq org-beamer-frame-level 2)

(provide 'config-ox-beamer)
;;; config-ox-beamer.el ends here
