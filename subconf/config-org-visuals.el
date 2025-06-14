;;; config-org-visuals.el --- Generated package (no.80) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.80) from my config.
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

(require 'org)

(add-hook 'org-mode-hook #'+org-pretty-mode)

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-fontify-quote-and-verse-blocks t)

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                             (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))

(setq org-inline-src-prettify-results '("⟨" . "⟩"))

(setq doom-themes-org-fontify-special-tags nil)

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'nerd-icons-red)
        (?B . 'nerd-icons-orange)
        (?C . 'nerd-icons-yellow)
        (?D . 'nerd-icons-green)
        (?E . 'nerd-icons-blue)))

(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :properties    "⚙"
                :end           "∎"
                :priority_a    #("⚑" 0 1 (face nerd-icons-red))
                :priority_b    #("⬆" 0 1 (face nerd-icons-orange))
                :priority_c    #("■" 0 1 (face nerd-icons-yellow))
                :priority_d    #("⬇" 0 1 (face nerd-icons-green))
                :priority_e    #("❓" 0 1 (face nerd-icons-blue))))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))

;; (package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")

;; (use-package org-pretty-tags
;; :config
;;  (setq org-pretty-tags-surrogate-strings
;;        `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
;;          ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
;;          ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
;;          ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
;;          ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
;;          ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
;;  (org-pretty-tags-global-mode))

(setq org-highlight-latex-and-related '(latex script entities))

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

(setq org-latex-preview-preamble
      (concat
       "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}"
       "\n% Custom font\n\\usepackage{arev}\n\n"
       "%% Maths-related packages
% More maths environments, commands, and symbols.
\\usepackage{amsmath, amssymb}
% Slanted fractions with \\sfrac{a}{b}, in text and maths.
\\usepackage{xfrac}
% Visually cancel expressions with \\cancel{value} and \\cancelto{expression}{value}
\\usepackage[makeroom]{cancel}
% Improvements on amsmath and utilities for mathematical typesetting
\\usepackage{mathtools}

% Deliminators
\\DeclarePairedDelimiter{\\abs}{\\lvert}{\\rvert}
\\DeclarePairedDelimiter{\\norm}{\\lVert}{\\rVert}

\\DeclarePairedDelimiter{\\ceil}{\\lceil}{\\rceil}
\\DeclarePairedDelimiter{\\floor}{\\lfloor}{\\rfloor}
\\DeclarePairedDelimiter{\\round}{\\lfloor}{\\rceil}

\\newcommand{\\RR}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{R}}{\\mathbb{R}^{#1}}}} % Real numbers
\\newcommand{\\NN}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{N}}{\\mathbb{N}^{#1}}}} % Natural numbers
\\newcommand{\\ZZ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Z}}{\\mathbb{Z}^{#1}}}} % Integer numbers
\\newcommand{\\QQ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Q}}{\\mathbb{Q}^{#1}}}} % Rational numbers
\\newcommand{\\CC}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{C}}{\\mathbb{C}^{#1}}}} % Complex numbers

% Easy derivatives
\\ProvideDocumentCommand\\dv{o m g}{%
  \\IfNoValueTF{#3}{%
    \\dv[#1]{}{#2}}{%
    \\IfNoValueTF{#1}{%
      \\frac{\\dd #2}{\\dd #3}%
    }{\\frac{\\dd[#1] #2}{\\dd {#3}^{#1}}}}}
% Easy partial derivatives
\\ExplSyntaxOn
\\ProvideDocumentCommand\\pdv{o m g}{%
  \\IfNoValueTF{#3}{\\pdv[#1]{}{#2}}%
  {\\ifnum\\clist_count:n{#3}<2
    \\IfValueTF{#1}{\\frac{\\partial^{#1} #2}{\\partial {#3}^{#1}}}%
    {\\frac{\\partial #2}{\\partial #3}}
    \\else
    \\frac{\\IfValueTF{#1}{\\partial^{#1}}{\\partial^{\\clist_count:n{#3}}}#2}%
    {\\clist_map_inline:nn{#3}{\\partial ##1 \\,}\\!}
    \\fi}}
\\ExplSyntaxOff

% Laplacian
\\DeclareMathOperator{\\Lap}{\\mathcal{L}}

% Statistics
\\DeclareMathOperator{\\Var}{Var} % varience
\\DeclareMathOperator{\\Cov}{Cov} % covarience
\\newcommand{\\EE}{\\ensuremath{\\mathbb{E}}} % expected value
\\DeclareMathOperator{\\E}{E} % expected value

% I prefer the slanted \\leq/\\geq
\\let\\barleq\\leq % Save them in case they're every wanted
\\let\\bargeq\\geq
\\renewcommand{\\leq}{\\leqslant}
\\renewcommand{\\geq}{\\geqslant}

% Redefine the matrix environment to allow for alignment
% via an optional argument, and use r as the default.
\\makeatletter
\\renewcommand*\\env@matrix[1][r]{\\hskip -\\arraycolsep%
    \\let\\@ifnextchar\\new@ifnextchar
    \\array{*\\c@MaxMatrixCols #1}}
\\makeatother

% Slanted roman \"d\" for derivatives
\\ifcsname pdfoutput\\endcsname
  \\ifnum\\pdfoutput>0 % PDF
    \\newsavebox\\diffdbox{}
    \\newcommand{\\slantedromand}{{\\mathpalette\\makesl{d}}}
    \\newcommand{\\makesl}[2]{%
      \\begingroup
      \\sbox{\\diffdbox}{$\\mathsurround=0pt#1\\mathrm{#2}$}%
      \\pdfsave%
      \\pdfsetmatrix{1 0 0.2 1}%
      \\rlap{\\usebox{\\diffdbox}}%
      \\pdfrestore%
      \\hskip\\wd\\diffdbox%
      \\endgroup}
  \\else % DVI
    \\newcommand{\\slantedromand}{d} % fallback
  \\fi
\\else % Also DVI
  \\newcommand{\\slantedromand}{d} % fallback
\\fi

% Derivative d^n, nicely spaced
\\makeatletter
\\newcommand{\\dd}[1][]{\\mathop{}\\!%
  \\expandafter\\ifx\\expandafter&\\detokenize{#1}&% \\ifstrempty from etoolbox
    \\slantedromand\\@ifnextchar^{\\hspace{0.2ex}}{\\hspace{0.1ex}}
  \\else
    \\slantedromand\\hspace{0.2ex}^{#1}
  \\fi}
\\makeatother

\\NewCommandCopy{\\daccent}{\\d}
\\renewcommand{\\d}{\\ifmmode\\dd\\else\\daccent\\fi}"))

;; Calibrated based on the TeX font and org-buffer font.
(plist-put org-format-latex-options :zoom 0.93)

(defvar +org-plot-term-size '(1050 . 650)
  "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")

(after! org-plot
  (defun +org-plot-generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)))

  (defun +org-plot-gnuplot-term-properties (_type)
    (format "background rgb '%s' size %s,%s"
            (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))

  (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
  (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))

(provide 'config-org-visuals)
;;; config-org-visuals.el ends here
