;;; config-ox-latex.el --- Generated package (no.84) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: August 07, 2026
;; Modified: August 07, 2026
;; Version: 2026.08.07
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.84) from my config.
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

(require 'ox-latex)
(require 'ox-latex-emoji)

;; org-latex-compilers = ("pdflatex" "xelatex" "lualatex"), which are the possible values for %latex
(setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(defun +org-export-latex-fancy-item-checkboxes (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
     (lambda (fullmatch)
       (concat "\\\\item[" (pcase (substring fullmatch 9 -3) ; content of capture group
                             ("square"   "\\\\checkboxUnchecked")
                             ("boxminus" "\\\\checkboxTransitive")
                             ("boxtimes" "\\\\checkboxChecked")
                             (_ (substring fullmatch 9 -3))) "]"))
     text)))

(add-to-list 'org-export-filter-item-functions
             '+org-export-latex-fancy-item-checkboxes)

(defvar org-latex-accent-colour t
  "Default accent colour behaviour for LaTeX export.
When t, derive the hue from the document title.
When nil, use grey (achromatic) shades.
When a string, interpret as a hue (\"0\"--\"360\"), colour name, or hex colour.")

(eval '(cl-pushnew '(:latex-accent-colour "ACCENT_COLOR" nil org-latex-accent-colour)
                   (org-export-backend-options (org-export-get-backend 'latex))))

(defun org-latex-accent-pastel-shades (hue chroma-factor lightnesses)
  "Generate a set of perceptually uniform pastel hex colours.
HUE is an angle in radians, CHROMA-FACTOR (0.0--1.0) scales the
saturation, and LIGHTNESSES is a list of OKLab L values."
  (mapcar
   (lambda (l)
     (let* ((chroma (* (- 1.02 l) l (+ 0.35 (* 0.45 chroma-factor))))
            (a (* chroma (cos hue)))
            (b (* chroma (sin hue)))
            (rgb (mapcar #'color-clamp (color-oklab-to-srgb l a b))))
       (substring (apply #'color-rgb-to-hex (append rgb (list 2))) 1)))
   lightnesses))

(defun org-latex-accent--parse (value info)
  "Parse accent colour VALUE into a (HUE . CHROMA-FACTOR) cons cell.
HUE is in radians, CHROMA-FACTOR is 0.0--1.0. INFO is the export
plist, used to access the document title for the default case.
VALUE may be t/nil (from the defvar default), or a string from the keyword."
  (pcase value
    ((or 'nil "no" "nil")
     (cons 0.0 (/ -0.35 0.45)))
    ((or 't "yes" "t" "")
     (let* ((title (org-html-plain-text
                    (org-element-interpret-data (plist-get info :title)) info))
            (hash (string-to-number (substring (sha1 (downcase title)) 0 12) 16)))
       (cons (* 2 float-pi (/ (+ (mod hash 320) 10) 360.0))
             (/ (mod hash 100) 100.0))))
    ((and (pred stringp)
          (guard (string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'" value)))
     (cons (* 2 float-pi (/ (string-to-number value) 360.0)) 1.0))
    ((pred stringp)
     (if-let ((rgb (color-name-to-rgb value)))
         (let* ((lab (apply #'color-srgb-to-oklab rgb))
                (l (car lab))
                (chroma (sqrt (+ (* (cadr lab) (cadr lab))
                                 (* (caddr lab) (caddr lab)))))
                ;; Invert: chroma = (1.02 - l) * l * (0.35 + 0.45 * factor)
                (l-scale (* (- 1.02 l) l))
                (factor (if (> l-scale 1e-6)
                            (min 1.0 (max 0.0 (/ (- (/ chroma l-scale) 0.35) 0.45)))
                          0.0)))
           (cons (atan (caddr lab) (cadr lab)) factor))
       (user-error "Unrecognised accent colour: %s" value)))
    (_ (user-error "Invalid accent colour value: %S" value))))

(defun org-latex-accent-colours (info)
  "Generate LaTeX colour definitions for the document accent colour.
Looks at INFO to find the accent option and document title."
  (let* ((accent-value (plist-get info :latex-accent-colour))
         (hue-chroma (org-latex-accent--parse accent-value info))
         (lightnesses '(0.25 0.5 0.7 0.96 0.97))
         (shades (org-latex-accent-pastel-shades
                  (car hue-chroma) (cdr hue-chroma) lightnesses))
         (no-accent-p (member accent-value '(nil "no" "nil"))))
    (apply #'format "\
\\providecolor{accentdark}{HTML}{%s}
\\providecolor{accent}{HTML}{%s}
\\providecolor{accentlight}{HTML}{%s}
\\providecolor{accentpale}{HTML}{%s}
\\providecolor{accentfaint}{HTML}{%s}\n\n"
           (if no-accent-p
               (append '("000000" "000000" "000000") (cdddr shades))
             shades))))

(org-export-update-features 'latex
  (accent-colour :snippet org-latex-accent-colours :order -1))

(after! ox-latex
  (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
         (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                article-sections))
         (big-chap-preamble "\\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
\\setkomafont{chapter}{\\normalfont\\Huge}
\\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterformat}{%
  \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
\\renewcommand*\\chapterlinesformat[3]{%
  \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
    \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
    \\hfill\\makebox[0pt][l]{#2}}"))
    (setcdr (assoc "article" org-latex-classes)
            `("\\documentclass{scrartcl}"
              ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("report" "\\documentclass{scrartcl}"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("book" ,(concat "\\documentclass[twoside=false]{scrbook}" big-chap-preamble)
                   ,@book-sections))
    (add-to-list 'org-latex-classes
                 `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@book-sections))))

(setq org-latex-tables-booktabs t
      org-latex-hyperref-template
      "\\providecolor{url}{HTML}{0077bb}
\\providecolor{link}{HTML}{882255}
\\providecolor{cite}{HTML}{999933}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite
}
\\urlstyle{same}"
      org-latex-reference-command "\\cref{%s}")

(defvar org-latex-maths-preamble
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
\\renewcommand{\\d}{\\ifmmode\\dd\\else\\daccent\\fi}"
  "Preamble that sets up a bunch of mathematical conveniences.")

(defvar org-latex-embed-files-preamble
  nil
  "Preamble that embeds files within the pdf.")

(defvar org-latex-koma-sections-preamble
  "\\renewcommand\\sectionformat{\\llap{\\color{accentlight}\\thesection\\autodot\\enskip}}
\\renewcommand\\subsectionformat{\\llap{\\color{accentlight}\\thesubsection\\autodot\\enskip}}
\\renewcommand\\subsubsectionformat{\\llap{\\color{accentlight}\\thesubsubsection\\autodot\\enskip}}
\\makeatletter
\\@ifundefined{scr@fnt@sectionentry}{}{\\addtokomafont{sectionentry}{\\color{accent}}}
\\@ifundefined{scr@fnt@chapterentry}{}{\\addtokomafont{chapterentry}{\\color{accent}}}
\\makeatother
\\addtokomafont{title}{\\color{accentdark}}
\\addtokomafont{author}{\\color{accentdark}}
\\addtokomafont{date}{\\color{accent}}
\\addtokomafont{disposition}{\\color{accentdark}}
\\addtokomafont{pagenumber}{\\color{accent}}"
  "Section customization for KOMA-Script classes.")

(defvar org-latex-caption-preamble
  "\\usepackage{subcaption}
\\usepackage[hypcap=true]{caption}
\\setkomafont{caption}{\\sffamily\\small}
\\setkomafont{captionlabel}{\\upshape\\bfseries}
\\captionsetup{justification=raggedright,singlelinecheck=true}
\\usepackage{capt-of} % required by Org"
  "Preamble that improves captions.")

(defvar org-latex-checkbox-preamble
  "\\newcommand{\\checkboxUnchecked}{$\\square$}
\\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
\\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}"
  "Preamble that improves checkboxes.")

(defvar org-latex-box-preamble
  "\\ExplSyntaxOn
\\NewCoffin\\SBXBaseline
\\NewCoffin\\SBXHeader
\\NewCoffin\\SBXContent
\\NewCoffin\\SBXSideRule
\\newbox\\SBXSplitBox
\\cs_new_protected:Nn \\simplebox_start:nnn {
  % #1 ding, #3 name, #4 label
  \\vcoffin_set:Nnn \\SBXHeader { \\linewidth - 1em } {
  \\noindent\\textcolor{#2}{#1}~\\textcolor{#2}{\\textbf{#3}}}
  \\vcoffin_set:Nnw \\SBXContent { \\linewidth - 1.5em }
}
\\cs_new_protected:Nn \\simplebox_split_content:n {
  % #1 name
  \\setbox\\SBXSplitBox = \\vbox:n { \\vbox_unpack_drop:N \\SBXContent }
  \\dim_set:Nn \\l_tmpa_dim { \\dim_eval:n { \\dim_min:nn { \\pagegoal } { \\textheight } - \\pagetotal - 2\\baselineskip } }
  \\setbox0 = \\vsplit\\SBXSplitBox to \\l_tmpa_dim
  \\vcoffin_set:Nnn \\SBXContent { \\CoffinWidth \\SBXContent } { \\box0 %
    \\vspace{-1.7\\baselineskip}
    \\noindent\\textcolor{#1}{\\textbf{\\ldots }}
    \\vspace*{-0.3\\baselineskip}}
}
\\cs_new_protected:Nn \\simplebox_split_refill:nnnn {
  % #1 ding, #2 ding offset, #3 name, #4 label
  \\simplebox_start:nnn {#1} {#3} {#4,\\space{}\\emph{continued}}
  \\vspace*{-0.2\\baselineskip}
  \\vbox_unpack_drop:N \\SBXSplitBox
  \\vcoffin_set_end:
}
\\cs_new_protected:Nn \\simplebox_typeset:nn {
    % #1 name, #2 ding offset
    \\vcoffin_set:Nnn \\SBXBaseline {0pt} {\\vbox{}}
    \\SetHorizontalCoffin\\SBXSideRule{\\color{#1}\\rule{1pt}{\\dim_eval:n { \\CoffinTotalHeight\\SBXContent + \\baselineskip }}}
    \\JoinCoffins*\\SBXContent[l,t]\\SBXSideRule[l,t](\\dim_eval:n {#2 - 1em}, \\dim_eval:n{\\baselineskip - 0.5em})
    \\JoinCoffins*\\SBXContent[l,t]\\SBXHeader[l,B](-1em, 0.5\\baselineskip)
    \\JoinCoffins*\\SBXBaseline[l,T]\\SBXContent[l,T]
    \\vspace{-0.5\\baselineskip}
    \\noindent\\TypesetCoffin\\SBXBaseline(\\dim_eval:n { 1em - #2 + 1pt }, 0pt)
    \\vspace*{\\CoffinTotalHeight\\SBXContent}
    \\vspace{-0.08em} % Why on earth is this needed for baseline alignment!?
}
\\cs_new_protected:Nn \\simplebox_typeset_breakable:nnnn {
    % #1 ding, #2 ding offset, #3 name, #4 label
    \\dim_set:Nn \\l_tmpa_dim {\\dim_eval:n { \\CoffinTotalHeight\\SBXContent + \\baselineskip }}
    \\dim_set:Nn \\l_tmpb_dim { \\dim_eval:n { \\dim_min:nn { \\pagegoal } { \\textheight } - \\pagetotal - \\baselineskip } }
    \\dim_compare:nNnTF {\\l_tmpa_dim} > {\\l_tmpb_dim} {
      \\simplebox_split_content:n {#3}
      \\simplebox_typeset:nn {#3} {#2}
      \\newpage
      \\simplebox_split_refill:nnnn {#1} {#2} {#3} {#4}
      \\simplebox_typeset_breakable:nnnn {#1} {#2} {#3} {#4}
    }{
      \\simplebox_typeset:nn {#3} {#2}
    }
}
\\NewDocumentCommand{\\defsimplebox}{O{\\ding{117}} O{0.35em} O{#1} O{#2} m m m}{
  % #1 ding, #2 ding offset, #3 alt-ding, #4 alt-ding offset,
  % #5 name, #6 colour, #7 default label
  \\definecolor{#5}{HTML}{#6}
  \\NewDocumentEnvironment{#5}{ O{#7} }{
    \\simplebox_start:nnn {#1} {#5} {##1}
  }{
    \\vcoffin_set_end:
    \\simplebox_typeset_breakable:nnnn {#3} {#4} {#5} {##1}
  }
}
\\ExplSyntaxOff"
  "Preamble that provides a macro for custom boxes.")

(defun org-latex-embed-extra-files ()
  "Return a string that uses embedfile to embed all tangled files."
  (mapconcat
   (lambda (file-desc)
     (format "\\IfFileExists{%1$s}{\\embedfile[desc=%2$s]{%1$s}}{}"
             (thread-last (car file-desc)
                          (replace-regexp-in-string "\\\\" "\\\\\\\\")
                          (replace-regexp-in-string "~" "\\\\string~"))
             (cdr file-desc)))
   (append
    (let (tangle-fspecs) ; All files being tangled to.
      (org-element-cache-map
       (lambda (src)
         (when (and (not (org-in-commented-heading-p nil src))
                    (not (org-in-archived-heading-p nil src)))
           (when-let ((lang (org-element-property :language src))
                      (params
                       (apply
                        #'org-babel-merge-params
                        (append
                         (org-with-point-at (org-element-property :begin src)
                           (org-babel-params-from-properties lang t))
                         (mapcar
                          (lambda (h)
                            (org-babel-parse-header-arguments h t))
                          (cons (org-element-property :parameters src)
                                (org-element-property :header src))))))
                      (tangle-value
                       (pcase (alist-get :tangle params)
                         ((and (pred stringp) (pred (string-match-p "^(.*)$")) expr)
                          (eval (read expr)))
                         (val val)))
                      (tangle-file
                       (pcase tangle-value
                         ((or "no" (guard (member (alist-get :export-embed params) '("no" "nil"))))
                          nil)
                         ("yes"
                          (file-name-with-extension
                           (file-name-nondirectory (buffer-file-name))
                           (or (alist-get lang org-babel-tangle-lang-exts nil nil #'equal)
                               lang)))
                         (val val))))
             (unless (assoc tangle-file tangle-fspecs)
               (push
                (cons tangle-file (format "Tangled %s file" lang))
                tangle-fspecs)))))
       :granularity 'element
       :restrict-elements '(src-block))
      (nreverse tangle-fspecs))
    (let (extra-files)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+embed:" nil t)
          (let* ((file-desc (split-string (org-element-property :value (org-element-at-point)) " :desc\\(?:ription\\)? ")))
            (push (cons (car file-desc) (or (cdr file-desc) "Extra file")) extra-files))))
      (nreverse extra-files)))
   "\n"))

(defvar org-latex-embed-files t
  "Embed the source .org, .tex, and any tangled files.")
(defvar org-latex-use-microtype t
  "Use the microtype pakage.")
(defvar org-latex-italic-quotes t
  "Make \"quote\" environments italic.")
(defvar org-latex-par-sep t
  "Vertically seperate paragraphs, and remove indentation.")

(org-export-update-features 'latex
  ((image caption)
   :condition "\\[\\[xkcd:"))

(org-export-update-features 'latex
  (maths
   :snippet org-latex-maths-preamble
   :order 0.2)
  (cleveref
   :condition "cref:\\|\\cref{\\|\\[\\[[^\\]+\n?[^\\]\\]\\]"
   :snippet "\\usepackage[capitalize]{cleveref}
% Fix for cleveref in order to work with long range of pages
% See https://tex.stackexchange.com/a/620066
\\makeatletter
\\newcommand*{\\@setcpagerefrange}[3]{%
  \\@@setcpagerefrange{#1}{#2}{cref}{#3}}
\\newcommand*{\\@setCpagerefrange}[3]{%
  \\@@setcpagerefrange{#1}{#2}{Cref}{#3}}
\\newcommand*{\\@setlabelcpagerefrange}[3]{%
  \\@@setcpagerefrange{#1}{#2}{labelcref}{#3}}
\\makeatother"
   :order 1)
  (caption
   :snippet org-latex-caption-preamble
   :order 2.1)
  (microtype
   :condition org-latex-use-microtype
   :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}"
   :order 0.1)
  (embed-files
   :condition org-latex-embed-files
   :snippet "\\usepackage[include]{embedall}"
   :order 70)
  (embed-source
   :condition t
   :when embed-files
   :snippet "\\IfFileExists{./\\jobname.org}{\\embedfile[desc=Primary source file]{\\jobname.org}}{}
\\IfFileExists{./\\jobname.tex}{\\embedfile[desc=The (generated) LaTeX source file]{\\jobname.tex}}{}"
   :no-precompile t
   :after embed-files
   :order 80)
  (embed-tangled
   :condition (and org-latex-embed-files
                   "^[ \t]*#\\+embed\\|^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC")
   :requires embed-files
   :snippet (org-latex-embed-extra-files)
   :no-precompile t
   :after (embed-source embed-files)
   :order 80)
  (acronym
   :condition "[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]"
   :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}"
   :order 0.4)
  (box-drawing
   :condition "[\u2500-\u259F]"
   :snippet "\\usepackage{pmboxdraw}"
   :order 0.05)
  (italic-quotes
   :condition (and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}")
   :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n"
   :order 0.5)
  (par-sep
   :condition org-latex-par-sep
   :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}"
   :order 0.5)
  (.pifont
   :snippet "\\usepackage{pifont}")
  (.xcoffins
   :snippet "\\usepackage{xcoffins}")
  (checkbox
   :condition "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]"
   :requires .pifont
   :snippet (concat (unless (memq 'maths features)
                      "\\usepackage{amssymb} % provides \\square")
                    org-latex-checkbox-preamble)
   :after .pifont)
  (.fancy-box
   :requires (.pifont .xcoffins)
   :snippet org-latex-box-preamble
   :after (.pifont .xcoffins))
  (box-warning
   :condition "^[ \t]*#\\+begin_warning\\|\\\\begin{warning}"
   :requires .fancy-box
   :snippet "\\defsimplebox{warning}{e66100}{Warning}"
   :after .fancy-box)
  (box-info
   :condition "^[ \t]*#\\+begin_info\\|\\\\begin{info}"
   :requires .fancy-box
   :snippet "\\defsimplebox{info}{3584e4}{Information}"
   :after .fancy-box)
  (box-notes
   :condition "^[ \t]*#\\+begin_notes\\|\\\\begin{notes}"
   :requires .fancy-box
   :snippet "\\defsimplebox{notes}{26a269}{Notes}"
   :after .fancy-box)
  (box-success
   :condition "^[ \t]*#\\+begin_success\\|\\\\begin{success}"
   :requires .fancy-box
   :snippet "\\defsimplebox{success}{26a269}{\\vspace{-\\baselineskip}}"
   :after .fancy-box)
  (box-error
   :condition "^[ \t]*#\\+begin_error\\|\\\\begin{error}"
   :requires .fancy-box
   :snippet "\\defsimplebox{error}{c01c28}{Important}"
   :after .fancy-box)
  (koma-sections
   :condition
   (let ((latex-class
          (assoc (plist-get info :latex-class) (plist-get info :latex-classes))))
     (and (cadr latex-class)
          (string-match-p "\\`\\\\documentclass\\(?:\\[.*\\]\\)?{scr" (cadr latex-class))
          (not (string-match-p "[[,]twocolumn[],]" (or (plist-get info :latex-class-options) "")))))
   :requires accent-colour
   :snippet org-latex-koma-sections-preamble)
  (toc-hidelinks
   :condition
   (or (plist-get info :with-toc)
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "\\tableofcontents" nil t)))
   :snippet "%% hide links styles in toc
\\NewCommandCopy{\\oldtoc}{\\tableofcontents}
\\renewcommand{\\tableofcontents}{\\begingroup\\hypersetup{hidelinks}\\oldtoc\\endgroup}"))

(setq org-latex-packages-alist
      '(("" "xcolor" t)))

(defvar org-latex-default-fontset 'alegreya
  "Fontset from `org-latex-fontsets' to use by default.
As cm (computer modern) is TeX's default, that causes nothing
to be added to the document.

If \"nil\" no custom fonts will ever be used.")

(eval '(cl-pushnew '(:latex-font-set nil "fontset" org-latex-default-fontset)
                   (org-export-backend-options (org-export-get-backend 'latex))))

(defun org-latex-fontset-entry ()
  "Get the fontset spec of the current file.
Has format \"name\" or \"name-style\" where 'name' is one of
the cars in `org-latex-fontsets'."
  (let ((fontset-spec
         (symbol-name
          (or (car (delq nil
                         (mapcar
                          (lambda (opt-line)
                            (plist-get (org-export--parse-option-keyword opt-line 'latex)
                                       :latex-font-set))
                          (cdar (org-collect-keywords '("OPTIONS"))))))
              org-latex-default-fontset))))
    (cons (intern (car (split-string fontset-spec "-")))
          (when (cadr (split-string fontset-spec "-"))
            (intern (concat ":" (cadr (split-string fontset-spec "-"))))))))

(defun org-latex-fontset (&rest desired-styles)
  "Generate a LaTeX preamble snippet which applies the current fontset for DESIRED-STYLES."
  (let* ((fontset-spec (org-latex-fontset-entry))
         (fontset (alist-get (car fontset-spec) org-latex-fontsets)))
    (if fontset
        (string-trim
         (concat
          (mapconcat
           (lambda (style)
             (when (plist-get fontset style)
               (concat (plist-get fontset style) "\n")))
           desired-styles
           "")
          (when (memq (cdr fontset-spec) desired-styles)
            (pcase (cdr fontset-spec)
              (:serif "\\renewcommand{\\familydefault}{\\rmdefault}\n")
              (:sans "\\renewcommand{\\familydefault}{\\sfdefault}\n")
              (:mono "\\renewcommand{\\familydefault}{\\ttdefault}\n")))))
      (error "Font-set %s is not provided in org-latex-fontsets" (car fontset-spec)))))

(org-export-update-features 'latex
  (custom-font
   :condition org-latex-default-fontset
   :snippet (org-latex-fontset :serif :sans :mono)
   :order 0)
  (custom-maths-font
   :condition t
   :when (custom-font maths)
   :snippet (org-latex-fontset :maths)
   :after (custom-font maths)
   :order 0))

(defvar org-latex-fontsets
  '((cm nil) ; computer modern
    (## nil) ; no font set
    (alegreya
     :serif "\\usepackage[osf]{Alegreya}"
     :sans "\\usepackage{AlegreyaSans}"
     :mono "\\usepackage[scale=0.88]{sourcecodepro}"
     :maths "\\let\\Bbbk\\relax\n\\usepackage[varbb]{newpxmath}")
    (biolinum
     :serif "\\usepackage[osf]{libertineRoman}"
     :sans "\\usepackage[sfdefault,osf]{biolinum}"
     :mono "\\usepackage[scale=0.88]{sourcecodepro}"
     :maths "\\usepackage[libertine,varvw]{newtxmath}")
    (fira
     :sans "\\usepackage[sfdefault,scale=0.85]{FiraSans}"
     :mono "\\usepackage[scale=0.80]{FiraMono}"
     :maths "\\usepackage{newtxsf} % change to firamath in future?")
    (kp
     :serif "\\usepackage{kpfonts}")
    (newpx
     :serif "\\usepackage{newpxtext}"
     :sans "\\usepackage{gillius}"
     :mono "\\usepackage[scale=0.9]{sourcecodepro}"
     :maths "\\let\\Bbbk\\relax\n\\usepackage[varbb]{newpxmath}")
    (noto
     :serif "\\usepackage[osf]{noto-serif}"
     :sans "\\usepackage[osf]{noto-sans}"
     :mono "\\usepackage[scale=0.96]{noto-mono}"
     :maths "\\usepackage{notomath}")
    (plex
     :serif "\\usepackage{plex-serif}"
     :sans "\\usepackage{plex-sans}"
     :mono "\\usepackage[scale=0.95]{plex-mono}"
     :maths "\\usepackage{newtxmath}") ; may be plex-based in future
    (source
     :serif "\\usepackage[osf,semibold]{sourceserifpro}"
     :sans "\\usepackage[osf,semibold]{sourcesanspro}"
     :mono "\\usepackage[scale=0.92]{sourcecodepro}"
     :maths "\\usepackage{newtxmath}") ; may be sourceserifpro-based in future
    (times
     :serif "\\usepackage{newtxtext}"
     :maths "\\usepackage{newtxmath}"))
  "Alist of fontset specifications.
Each car is the name of the fontset (which cannot include \"-\").

Each cdr is a plist with (optional) keys :serif, :sans, :mono, and :maths.
A key's value is a LaTeX snippet which loads such a font.")

(org-export-update-features 'latex
  (alegreya-typeface
   :condition (string= (car (org-latex-fontset-entry)) "alegreya")
   :snippet nil)
  (alegreya-tabular-figures
   :condition t
   :when (alegreya-typeface table)
   :snippet "\
\\makeatletter
% tabular lining figures in tables
\\renewcommand{\\tabular}{\\AlegreyaTLF\\let\\@halignto\\@empty\\@tabular}
\\makeatother"
   :after custom-font
   :order 0.5))

(org-export-update-features 'latex
  (alegreya-latex-symbol
    :condition "LaTeX"
    :when alegreya-typeface
    :snippet "\
\\makeatletter
% Kerning around the A needs adjusting
\\DeclareRobustCommand{\\LaTeX}{L\\kern-.24em%
        {\\sbox\\z@ T%
         \\vbox to\\ht\\z@{\\hbox{\\check@mathfonts
                              \\fontsize\\sf@size\\z@
                              \\math@fontsfalse\\selectfont
                              A}%
                        \\vss}%
        }%
        \\kern-.10em%
        \\TeX}
\\makeatother"
    :after alegreya-typeface
    :order 0.5))

(defvar org-latex-cover-page 'auto
  "When t, use a cover page by default.
When auto, use a cover page when the document's wordcount exceeds
`org-latex-cover-page-wordcount-threshold'.

Set with #+option: coverpage:{yes,auto,no} in org buffers.")
(defvar org-latex-cover-page-wordcount-threshold 5000
  "Document word count at which a cover page will be used automatically.
This condition is applied when cover page option is set to auto.")
(defvar org-latex-subtitle-coverpage-format "\\\\\\bigskip\n\\LARGE\\mdseries\\itshape\\color{black!80} %s\\par"
  "Variant of `org-latex-subtitle-format' to use with the cover page.")
(defvar org-latex-cover-page-maketitle
  "\\usepackage{tikz}
\\usetikzlibrary{shapes.geometric}
\\usetikzlibrary{calc}

\\newsavebox\\covericon
\\begin{lrbox}{\\covericon}
  \\begin{tikzpicture}[y=1.40pt, x=1.40pt, inner sep=0pt, outer sep=0pt]
    \\path[fill=accentpale] (9.2, 17.5).. controls (9.0, 17.5) and (8.7, 17.5) ..  (8.5, 17.5).. controls (5.3, 17.2) and
    (5.4, 16.2) .. (3.6, 15.6).. controls (1.6, 15.3) and (1.6, 12.1) .. (3.1, 12.7).. controls (3.6, 12.7) and
    (4.2, 12.9) .. (4.9, 13.1).. controls (6.0, 12.5) and (6.7, 11.8) .. (7.2, 11.1)..  controls (7.0, 11.2) and
    (6.6, 11.5) .. (4.7, 11.8).. controls (4.0, 12.0) and (2.9, 12.1) .. (2.0, 12.0).. controls (0.4, 11.4) and
    (3.8, 10.5) .. (2.2, 10.0).. controls (0.7, 9.9) and (-0.1, 7.5) .. (1.7, 8.3).. controls (2.4, 8.7) and
    (3.7, 9.0) .. (4.5, 9.0).. controls (4.9, 8.8) and (5.3, 8.5) .. (5.7, 8.3).. controls (4.9, 8.2) and
    (3.9, 8.1) .. (3.2, 8.2).. controls (2.5, 7.3) and (0.4, 6.4) .. (0.7, 5.3).. controls (1.7, 4.6) and
    (2.3, 6.4) .. (3.2, 6.4).. controls (4.4, 5.8) and (6.4, 6.3) .. (7.1, 6.8).. controls (7.7, 6.5) and
    (7.8, 5.8) .. (8.2, 5.3)arc(4.0:4.2:203.7 and -203.7).. controls (7.8, 4.7) and
    (7.1, 4.7) .. (6.6, 4.6).. controls (6.9, 4.7) and (7.3, 4.7) .. (7.4, 5.0).. controls (7.4, 5.5) and
    (3.9, 6.0) .. (2.2, 4.6).. controls (1.9, 3.6) and (3.7, 4.5) .. (4.4, 4.2).. controls (3.7, 3.3) and
    (3.4, 2.9) .. (5.1, 3.1).. controls (6.5, 3.3) and (7.0, 3.4) .. (6.0, 4.2) -- (6.0, 4.2).. controls (6.7, 4.2) and
    (7.4, 4.6) .. (8.1, 4.0).. controls (8.0, 3.3) and (8.0, 2.7) ..  (7.9, 2.1).. controls (5.0, 2.3) and
    (4.1, 0.7) .. (7.5, 0.7).. controls (13.2, 0.5) and (12.4, 2.0) .. (10.0, 1.9).. controls (9.8, 2.0) and
    (9.6, 2.0) ..  (9.4, 2.0).. controls (9.3, 2.5) and (9.5, 3.2) .. (9.6, 3.4).. controls (10.1, 3.7) and
    (10.5, 3.8) .. (10.8, 3.7).. controls (10.7, 3.5) and (10.7, 3.3) ..  (11.1, 2.9).. controls (12.2, 2.1) and
    (13.1, 2.9) .. (15.1, 1.9).. controls (16.3, 1.5) and (18.8, 3.1) .. (16.0, 4.0).. controls (15.2, 4.3) and
    (17.8, 6.6) .. (15.7, 6.6) -- (14.1, 6.6).. controls (13.3, 6.6) and (11.2, 6.9) ..  (12.2, 6.0).. controls (11.0, 6.5) and
    (11.0, 4.7) .. (9.8, 4.3).. controls (10.1, 5.6) and (10.6, 7.0) .. (11.5, 7.9).. controls (11.9, 7.8) and
    (12.2, 7.9) .. (12.0, 7.4).. controls (11.0, 6.3) and (14.1, 6.9) .. (14.7, 7.8)..  controls (17.1, 7.7) and
    (17.1, 9.0) .. (14.6, 9.1).. controls (13.2, 9.2) and (12.1, 9.4) .. (10.4, 9.0).. controls (9.5, 8.7) and
    (10.3, 8.4) .. (11.1, 8.2).. controls (10.1, 7.2) and (10.0, 5.4) .. (9.2, 4.5).. controls (9.2, 5.2) and
    (9.1, 5.9) .. (9.1, 6.7).. controls (9.5, 8.0) and (10.0, 9.1) .. (10.8, 9.8).. controls (12.0, 9.8) and
    (13.1, 9.7) .. (14.1, 9.6).. controls (16.0, 9.3) and (15.6, 11.4) .. (14.6, 12.4).. controls (13.2, 13.2) and
    (11.0, 13.3) .. (9.6, 12.9).. controls (9.5, 12.5) and (9.3, 11.8) .. (9.0, 11.5).. controls (8.9, 11.8) and
    (8.9, 12.1) .. (8.9, 12.4).. controls (8.9, 12.9) and (9.3, 13.5) .. (9.5, 13.9)arc(282.6:290.3:1.8 and -1.8).. controls (11.0, 13.1) and
    (13.0, 12.8) .. (14.1, 13.5).. controls (15.4, 14.6) and (12.8, 14.7) .. (12.1, 14.9).. controls (12.4, 16.6) and
    (10.7, 17.5) .. (9.2, 17.5)(8.8, 14.0)..  controls (8.8, 13.6) and (8.7, 13.2) .. (8.7, 12.7).. controls (8.4, 13.1) and
    (8.0, 13.2) .. (7.9, 13.9).. controls (8.2, 13.9) and (8.5, 14.0) .. (8.8, 14.0)(6.7, 13.6).. controls (7.6, 12.9) and
    (8.3, 12.2) .. (8.5, 11.2) -- (8.5, 11.1).. controls (8.2, 11.0) and (7.9, 10.9) .. (7.7, 10.8).. controls (7.1, 11.7) and
    (6.1, 12.5) .. (6.0, 13.4).. controls (6.2, 13.5) and (6.5, 13.5) ..  (6.7, 13.6)(5.6, 11.3) -- (6.4, 11.1).. controls (6.6, 11.1) and
    (6.9, 10.9) ..  (7.0, 10.8).. controls (6.7, 10.8) and (6.3, 10.8) .. (6.0, 11.0).. controls (5.9, 11.1) and
    (5.7, 11.2) .. (5.6, 11.3)(10.2, 9.8).. controls (9.7, 9.2) and (9.3, 8.5) .. (9.1, 7.7)arc(177.9:178.2:378.8 and -378.8).. controls (9.4, 9.8) and
    (9.8, 9.8) .. (10.2, 9.8)(8.4, 9.7).. controls (8.4, 9.3) and (8.4, 9.0) ..  (8.4, 8.6) -- (8.1, 9.7).. controls (8.2, 9.7) and
    (8.3, 9.7) .. (8.4, 9.7)(7.6, 9.6).. controls (7.8, 8.9) and (8.1, 8.4) .. (8.2, 7.7).. controls (8.2, 7.1) and
    (8.3, 6.7) .. (8.2, 6.2).. controls (8.1, 6.6) and (7.9, 6.9) .. (7.5, 7.2).. controls (7.8, 7.5) and
    (8.1, 8.0) .. (7.9, 8.2).. controls (7.3, 8.3) and (6.7, 8.3) .. (6.2, 8.3).. controls (5.5, 8.6) and
    (5.2, 8.8) .. (5.0, 9.1).. controls (5.4, 9.1) and (5.6, 9.1) .. (5.9, 9.2)(12.6, 5.0).. controls (13.1, 4.5) and
    (11.8, 4.4) .. (11.3, 4.0).. controls (10.9, 4.1) and (10.5, 4.2) .. (10.1, 4.1).. controls (11.2, 4.7) and (11.3, 6.2) .. (12.2, 5.6);
  \\end{tikzpicture}
\\end{lrbox}

\\makeatletter
\\ExplSyntaxOn

\\cs_new:Npn \\cover_date_format:nnn #1#2#3 {%
  \\LARGE #1 \\\\ \\large
  \\str_case:nnF {#2}
    {{01}{Jan} {02}{Feb} {03}{Mar} {04}{Apr} {05}{May} {06}{Jun}
     {07}{Jul} {08}{Aug} {09}{Sep} {10}{Oct} {11}{Nov} {12}{Dec}}
    {#2}~\\int_eval:n {#3}
}

\\cs_new:Npn \\cover_print_date:n #1
  {\\tl_if_blank:nTF {#1} {} {\\cover_print_date_aux:n {#1}}}

\\str_new:N \\l_cover_date_str
\\cs_new:Npn \\cover_print_date_aux:n #1 {%
  \\str_set:Nn \\l_cover_date_str {#1}
  \\bool_if:nTF
    {\\int_compare_p:n {\\str_count:N \\l_cover_date_str = 10} &&
     \\str_if_eq_p:ee {\\str_item:Nn \\l_cover_date_str {5}} {-} &&
     \\str_if_eq_p:ee {\\str_item:Nn \\l_cover_date_str {8}} {-}}
    {\\exp_args:Nee \\cover_date_format:nnn
        {\\str_range:Nnn \\l_cover_date_str {1} {4}}
        {\\str_range:Nnn \\l_cover_date_str {6} {7}}
        {\\str_range:Nnn \\l_cover_date_str {9} {10}}}
    {#1}
}

\\NewDocumentCommand\\coverprintdate{m}{\\cover_print_date:n {#1}}

\\ExplSyntaxOff

\\g@addto@macro\\tableofcontents{\\clearpage}
\\renewcommand\\maketitle{
  \\thispagestyle{empty}
  \\hyphenpenalty=10000 % hyphens look bad in titles
  \\renewcommand{\\baselinestretch}{1.1}
  \\NewCommandCopy{\\oldtoday}{\\today}
  \\renewcommand{\\today}{\\number\\year-\\two@digits{\\month}-\\two@digits{\\day}}
  \\begin{tikzpicture}[remember picture,overlay]
    %% Background Polygons %%
    \\foreach \\i in {2.5,...,22} % bottom left
    {\\node[rounded corners,accentfaint!70,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.west)+(2.5,-4.2)$) {} ;}
    \\foreach \\i in {0.5,...,22} % top left
    {\\node[rounded corners,accentfaint,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {} ;}
    \\node[rounded corners,fill=accentfaint!80,regular polygon,regular polygon sides=6, minimum size=5.5 cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {};
    \\foreach \\i in {0.5,...,24} % top right
    {\\node[rounded corners,accentfaint!40,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {} ;}
    \\node[fill=accentfaint!60,rounded corners,regular polygon,regular polygon sides=6, minimum size=2.5 cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {};
    \\foreach \\i in {21,...,3} % bottom right
    {\\node[accentfaint!60,rounded corners,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {} ;}
    \\node[fill=accentfaint!60,rounded corners,regular polygon,regular polygon sides=6, minimum size=2 cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {};
    \\node[align=center, scale=1.4] at ($(current page.south east)+(-1.5,0.75)$) {\\usebox\\covericon};
    %% Text %%
    \\node[left, align=right, text=accentdark, text width=0.8\\paperwidth, minimum height=3cm, rounded corners,font=\\Huge\\bfseries] at ($(current page.north east)+(-2,-8.5)$)
    {\\@title};
    \\node[left, align=right, text=accentdark, text width=0.8\\paperwidth, minimum height=2cm, rounded corners, font=\\Large] at ($(current page.north east)+(-2,-11.8)$)
    {\\scshape \\@author};
    \\renewcommand{\\baselinestretch}{0.75}
    \\node[align=center,rounded corners,fill=accentfaint,text=accent,regular polygon,regular polygon sides=6, minimum size=2.5 cm,inner sep=0, font=\\Large\\bfseries ] at ($(current page.west)+(2.5,-4.2)$)
    {\\coverprintdate{\\@date}};
  \\end{tikzpicture}
  \\let\\today\\oldtoday
  \\clearpage}
\\makeatother"
  "LaTeX preamble snippet that sets \\maketitle to produce a cover page.")

(eval '(cl-pushnew '(:latex-cover-page nil "coverpage" org-latex-cover-page)
                   (org-export-backend-options (org-export-get-backend 'latex))))

(defun org-latex-cover-page-p ()
  "Whether a cover page should be used when exporting this Org file."
  (let ((latex-backend (org-export-get-backend 'latex)))
    (pcase (or (car
                (delq nil
                      (mapcar
                       (lambda (opt-line)
                         (plist-get (org-export--parse-option-keyword opt-line latex-backend) :latex-cover-page))
                       (cdar (org-collect-keywords '("OPTIONS"))))))
               org-latex-cover-page)
      ((or 't 'yes) t)
      ('auto (when (> (count-words (point-min) (point-max)) org-latex-cover-page-wordcount-threshold) t))
      (_ nil))))

(defadvice! org-latex-set-coverpage-subtitle-format-a (contents info)
  "Set the subtitle format when a cover page is being used."
  :before #'org-latex-template
  (when (org-latex-cover-page-p)
    (setf info (plist-put info :latex-subtitle-format org-latex-subtitle-coverpage-format))))

(org-export-update-features 'latex
  (cover-page
   :condition (org-latex-cover-page-p)
   :requires accent-colour
   :snippet org-latex-cover-page-maketitle
   :order 9))

(defvar org-latex-condense-lists t
  "Reduce the space between list items.")
(defvar org-latex-condensed-lists
  "\\newcommand{\\setuplistspacing}{\\setlength{\\itemsep}{-0.5ex}\\setlength{\\parskip}{1.5ex}\\setlength{\\parsep}{0pt}}
\\let\\olditem\\itemize\\renewcommand{\\itemize}{\\olditem\\setuplistspacing}
\\let\\oldenum\\enumerate\\renewcommand{\\enumerate}{\\oldenum\\setuplistspacing}
\\let\\olddesc\\description\\renewcommand{\\description}{\\olddesc\\setuplistspacing}"
  "LaTeX preamble snippet that reduces the space between list items.")

(org-export-update-features 'latex
  (condensed-lists
   :condition (and org-latex-condense-lists "^[ \t]*[-+]\\|^[ \t]*[1Aa][.)] ")
   :snippet org-latex-condensed-lists
   :order 0.7))

(use-package! engrave-faces-latex
  :after ox-latex)

(setq org-latex-listings 'engraved
      org-latex-engraved-theme 'doom-one-light)

(org-export-update-features 'latex
  (no-protrusion-in-code
   :condition t
   :when (microtype engraved-code)
   :snippet "\\ifcsname Code\\endcsname\n  \\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}\n\\fi"
   :after (engraved-code microtype)))

(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))

(defadvice! org-latex-pick-compiler (_contents info)
  :before #'org-latex-template
  :before #'org-beamer-template
  (when (and (memq 'code (plist-get info :features))
             (memq 'julia-code (plist-get info :features))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "[^\x00-\x7F\u200b]" nil t)))
    (setf info (plist-put
                (if (member #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output))
                    (plist-put info :filter-final-output
                               (delq #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output)))
                  info)
                :latex-compiler "lualatex"))))

(defvar org-latex-julia-mono-fontspec
  "\\ifcsname directlua\\endcsname
  \\usepackage{fontspec}
  \\newfontfamily\\JuliaMono{JuliaMono-Regular.ttf}[Path=/usr/share/fonts/truetype/, Extension=.ttf]
  \\newfontface\\JuliaMonoRegular{JuliaMono-Regular}
  \\setmonofont{JuliaMonoRegular}[Contextuals=Alternate, Scale=MatchLowercase]
\\fi"
  "LaTeX preamble snippet that sets LuaLaTeX's fontspec to use Julia Mono.")

(org-export-update-features 'latex
  (julia-code
   :condition "^[ \t]*#\\+begin_src julia\\|^[ \t]*#\\+BEGIN_SRC julia\\|src_julia"
   :when code
   :snippet org-latex-julia-mono-fontspec
   :after custom-font
   :order 0)
  (microtype-lualatex
   :condition t
   :when (microtype julia-code)
   :prevents microtype
   :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,factor=2000]{microtype}\n"
   :order 0.1)
  (custom-font-no-mono
   :condition t
   :when julia-code
   :prevents custom-font
   :snippet (org-latex-fontset :serif :sans)
   :order 0))

(defvar +org-pdflatex-inputenc-encoded-chars
  "[[:ascii:]\u00A0-\u01F0\u0218-\u021BȲȳȷˆˇ˜˘˙˛˝\u0400-\u04FFḂḃẞ\u200B\u200C\u2010-\u201E†‡•…‰‱‹›※‽⁄⁎⁒₡₤₦₩₫€₱℃№℗℞℠™Ω℧℮←↑→↓〈〉␢␣◦◯♪⟨⟩Ḡḡ\uFB00-\uFB06\u2500-\u259F]")

(defun +org-latex-replace-non-ascii-chars (text backend info)
  "Replace non-ascii chars with \\char\"XYZ forms."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string= (plist-get info :latex-compiler) "pdflatex"))
    (let (case-replace)
      (replace-regexp-in-string "[^[:ascii:]]"
                                (lambda (nonascii)
                                  (if (or (string-match-p +org-pdflatex-inputenc-encoded-chars nonascii)
                                          (string-match-p org-latex-emoji--rx nonascii))
                                      nonascii
                                    (or (cdr (assoc nonascii +org-latex-non-ascii-char-substitutions))
                                        "¿")))
                                text))))

(add-to-list 'org-export-filter-plain-text-functions #'+org-latex-replace-non-ascii-chars t)

(defvar +org-latex-non-ascii-char-substitutions
   '(("ɑ" . "\\\\(\\\\alpha\\\\)")
     ("β" . "\\\\(\\\\beta\\\\)")
     ("γ" . "\\\\(\\\\gamma\\\\)")
     ("δ" . "\\\\(\\\\delta\\\\)")
     ("ε" . "\\\\(\\\\epsilon\\\\)")
     ("ϵ" . "\\\\(\\\\varepsilon\\\\)")
     ("ζ" . "\\\\(\\\\zeta\\\\)")
     ("η" . "\\\\(\\\\eta\\\\)")
     ("θ" . "\\\\(\\\\theta\\\\)")
     ("ϑ" . "\\\\(\\\\vartheta\\\\)")
     ("ι" . "\\\\(\\\\iota\\\\)")
     ("κ" . "\\\\(\\\\kappa\\\\)")
     ("λ" . "\\\\(\\\\lambda\\\\)")
     ("μ" . "\\\\(\\\\mu\\\\)")
     ("ν" . "\\\\(\\\\nu\\\\)")
     ("ξ" . "\\\\(\\\\xi\\\\)")
     ("π" . "\\\\(\\\\pi\\\\)")
     ("ϖ" . "\\\\(\\\\varpi\\\\)")
     ("ρ" . "\\\\(\\\\rho\\\\)")
     ("ϱ" . "\\\\(\\\\varrho\\\\)")
     ("σ" . "\\\\(\\\\sigma\\\\)")
     ("ς" . "\\\\(\\\\varsigma\\\\)")
     ("τ" . "\\\\(\\\\tau\\\\)")
     ("υ" . "\\\\(\\\\upsilon\\\\)")
     ("ϕ" . "\\\\(\\\\phi\\\\)")
     ("φ" . "\\\\(\\\\varphi\\\\)")
     ("ψ" . "\\\\(\\\\psi\\\\)")
     ("ω" . "\\\\(\\\\omega\\\\)")
     ("Γ" . "\\\\(\\\\Gamma\\\\)")
     ("Δ" . "\\\\(\\\\Delta\\\\)")
     ("Θ" . "\\\\(\\\\Theta\\\\)")
     ("Λ" . "\\\\(\\\\Lambda\\\\)")
     ("Ξ" . "\\\\(\\\\Xi\\\\)")
     ("Π" . "\\\\(\\\\Pi\\\\)")
     ("Σ" . "\\\\(\\\\Sigma\\\\)")
     ("Υ" . "\\\\(\\\\Upsilon\\\\)")
     ("Φ" . "\\\\(\\\\Phi\\\\)")
     ("Ψ" . "\\\\(\\\\Psi\\\\)")
     ("Ω" . "\\\\(\\\\Omega\\\\)")
     ("א" . "\\\\(\\\\aleph\\\\)")
     ("ב" . "\\\\(\\\\beth\\\\)")
     ("ד" . "\\\\(\\\\daleth\\\\)")
     ("ג" . "\\\\(\\\\gimel\\\\)")))

(defvar +org-latex-abbreviations
  '(;; Latin
    "cf." "e.g." "etc." "et al." "i.e." "v." "vs." "viz." "n.b."
    ;; Corperate
    "inc." "govt." "ltd." "pty." "dept."
    ;; Temporal
    "est." "c."
    ;; Honorifics
    "Prof." "Dr." "Mr." "Mrs." "Ms." "Miss." "Sr." "Jr."
    ;; Components of a work
    "ed." "vol." "sec." "chap." "pt." "pp." "op." "no."
    ;; Common usage
    "approx." "misc." "min." "max.")
  "A list of abbreviations that should be spaced correctly when exporting to LaTeX.")

(defun +org-latex-correct-latin-abbreviation-spaces (text backend _info)
  "Normalise spaces after Latin abbreviations."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string (rx (group (or line-start space)
                                         (regexp (regexp-opt-group +org-latex-abbreviations)))
                                  (or line-end space))
                              "\\1\\\\ "
                              text)))

(add-to-list 'org-export-filter-paragraph-functions #'+org-latex-correct-latin-abbreviation-spaces t)

(defvar org-latex-extra-special-string-regexps
  '(("<->" . "\\\\(\\\\leftrightarrow{}\\\\)")
    ("->" . "\\\\textrightarrow{}")
    ("<-" . "\\\\textleftarrow{}")))

(defun org-latex-convert-extra-special-strings (string)
  "Convert special characters in STRING to LaTeX."
  (dolist (a org-latex-extra-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defadvice! org-latex-plain-text-extra-special-a (orig-fn text info)
  "Make `org-latex-plain-text' handle some extra special strings."
  :around #'org-latex-plain-text
  (let ((output (funcall orig-fn text info)))
    (when (plist-get info :with-special-strings)
      (setq output (org-latex-convert-extra-special-strings output)))
    output))

(setq org-latex-text-markup-alist
      '((bold . "\\textbf{%s}")
        (code . protectedtexttt)
        (italic . "\\emph{%s}")
        (strike-through . "\\sout{%s}")
        (underline . "\\uline{%s}")
        (verbatim . verb)))

(setq org-required-latex-packages
      '("adjustbox" "accsupp" "amsmath" "booktabs" "cancel" "capt-of"
	"caption" "cleveref" "embedall" "etoolbox" "float" "fontenc"
	"fvextra" "graphicx" "hanging" "hyperref" "inputenc"
	"longtable" "mathalpha" "mathtools" "microtype" "pdfx"
	"pifont" "pmboxdraw" "preview" "scrbase" "scrextend" "siunitx"
	"soul" "subcaption" "svg" "tcolorbox" "textcomp" "tikz"
	"transparent" "xcoffins" "xcolor" "xparse" "Alegreya" "arev"
	"arevmath" "biolinum" "FiraMono" "FiraSans" "fourier"
	"gillius" "kpfonts" "libertine" "newpxmath" "newpxtext"
	"newtxmath" "newtxtext" "newtxsf" "noto" "notomath"
	"plex-mono" "plex-sans" "plex-serif" "sourcecodepro"
	"sourcesanspro" "sourceserifpro"))
;; Detected missing LaTeX packages: %s
(defun check-for-latex-packages (packages)
  (delq nil
	(mapcar
	 (lambda (package)
	   (unless
	       (= 0
		  (call-process "kpsewhich" nil nil nil
				(concat package ".sty")))
	     package))
	 packages)))
(defun +org-warn-about-missing-latex-packages (&rest _)
  (message "Checking for missing LaTeX packages...") (sleep-for 0.4)
  (if-let
      (missing-pkgs
       (check-for-latex-packages org-required-latex-packages))
      (message "%s You are missing the following LaTeX packages: %s."
	       (propertize "Warning!" 'face '(bold warning))
	       (mapconcat
		(lambda (pkg)
		  (propertize pkg 'face 'font-lock-variable-name-face))
		missing-pkgs ", "))
    (message
     "%s You have all the required LaTeX packages. Run %s to make this message go away."
     (propertize "Success!" 'face '(bold success))
     (propertize "doom sync" 'face 'font-lock-keyword-face))
    (advice-remove 'org-latex-export-to-pdf
		   #'+org-warn-about-missing-latex-packages))
  (sleep-for 1))
(advice-add 'org-latex-export-to-pdf :before
	    #'+org-warn-about-missing-latex-packages)


(provide 'config-ox-latex)
;;; config-ox-latex.el ends here
