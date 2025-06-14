;;; ox-latex-emoji.el --- Generated package (no.84) from my config -*- lexical-binding: t; -*-
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

(defvar org-latex-emoji--rx
  (let (emojis)
    (map-char-table
     (lambda (char set)
       (when (eq set 'emoji)
         (push (copy-tree char) emojis)))
     char-script-table)
    (rx-to-string `(any ,@emojis)))
  "A regexp to find all emoji-script characters.")

(defconst org-latex-emoji-base-dir
  (expand-file-name "emojis/" doom-cache-dir)
  "Directory where emojis should be saved and look for.")

(defvar org-latex-emoji-sets
  '(("twemoji" :url "https://github.com/jdecked/twemoji/archive/refs/tags/v15.1.0.zip"
     :folder "twemoji-15.1.0/assets/svg" :height "1.8ex" :offset "-0.3ex")
    ("twemoji-bw" :url "https://github.com/youdly/twemoji-color-font/archive/refs/heads/v11-release.zip"
     :folder "twemoji-color-font-11-release/assets/builds/svg-bw" :height "1.8ex" :offset "-0.3ex")
    ("openmoji" :url "https://github.com/hfg-gmuend/openmoji/releases/latest/download/openmoji-svg-color.zip"
     :height "2.2ex" :offset "-0.45ex")
    ("openmoji-bw" :url "https://github.com/hfg-gmuend/openmoji/releases/latest/download/openmoji-svg-black.zip"
     :height "2.2ex" :offset "-0.45ex")
    ("emojione" :url "https://github.com/joypixels/emojione/archive/refs/tags/v2.2.7.zip"
     :folder "emojione-2.2.7/assets/svg") ; Warning, poor coverage
    ("noto" :url "https://github.com/googlefonts/noto-emoji/archive/refs/tags/v2.038.zip"
     :folder "noto-emoji-2.038/svg" :file-regexp "^emoji_u\\([0-9a-f_]+\\)"
     :height "2.0ex" :offset "-0.3ex"))
  "An alist of plistst of emoji sets.
Specified with the minimal form:
  (\"SET-NAME\" :url \"URL\")
The following optional parameters are supported:
  :folder (defaults to \"\")
  The folder within the archive where the emojis exist.
  :file-regexp (defaults to nil)
  Pattern with the emoji code point as the first capture group.
  :height (defaults to \"1.8ex\")
  Height of the emojis to be used.
  :offset (defaults to \"-0.3ex\")
  Baseline offset of the emojis.")

(defconst org-latex-emoji-keyword
  "LATEX_EMOJI_SET"
  "Keyword used to set the emoji set from `org-latex-emoji-sets'.")

(defvar org-latex-emoji-preamble "\\usepackage{accsupp}
% The transparent package is also needed, but will be loaded later.
\\newsavebox\\emojibox

\\NewDocumentCommand\\DeclareEmoji{m m}{% UTF-8 codepoint, UTF-16 codepoint
  \\DeclareUnicodeCharacter{#1}{%
    \\sbox\\emojibox{\\raisebox{OFFSET}{%
        \\includegraphics[height=HEIGHT]{EMOJI-FOLDER/#1}}}%
    \\usebox\\emojibox
    \\llap{%
      \\resizebox{\\wd\\emojibox}{\\height}{%
        \\BeginAccSupp{method=hex,unicode,ActualText=#2}%
        \\texttransparent{0}{X}%
        \\EndAccSupp{}}}}}"
  "LaTeX preamble snippet that will allow for emojis to be declared.
Contains the string \"EMOJI-FOLDER\" which should be replaced with
the path to the emoji folder.")

(defun org-latex-emoji-utf16 (char)
  "Return the pair of UTF-16 surrogates that represent CHAR."
  (list
   (+ #xD7C0 (ash char -10))
   (+ #xDC00 (logand char #x03FF))))

(defun org-latex-emoji-declaration (char)
  "Construct the LaTeX command declaring CHAR as an emoji."
  (format "\\DeclareEmoji{%X}{%s} %% %s"
          char
          (if (< char #xFFFF)
              (format "%X" char)
            (apply #'format "%X%X" (org-latex-emoji-utf16 char)))
          (capitalize (get-char-code-property char 'name))))

(defun org-latex-emoji-fill-preamble (emoji-folder &optional height offset svg-p)
  "Fill in `org-latex-emoji-preamble' with EMOJI-FOLDER, HEIGHT, and OFFSET.
If SVG-P is set \"includegraphics\" will be replaced with \"includesvg\"."
  (let* (case-fold-search
         (filled-preamble
          (replace-regexp-in-string
           "HEIGHT"
           (or height "1.8ex")
           (replace-regexp-in-string
            "OFFSET"
            (or offset "-0.3ex")
            (replace-regexp-in-string
             "EMOJI-FOLDER"
             (directory-file-name
              (if (getenv "HOME")
                  (replace-regexp-in-string
                   (regexp-quote (getenv "HOME"))
                   "\\string~"
                   emoji-folder t t)
                emoji-folder))
             org-latex-emoji-preamble t t)
            t t)
           t t)))
    (if svg-p
        (replace-regexp-in-string
         "includegraphics" "includesvg"
         filled-preamble t t)
      filled-preamble)))

(defun org-latex-emoji-setup (&optional info)
  "Construct a preamble snippet to set up emojis based on INFO."
  (let* ((emoji-set
          (or (org-element-map
                  (plist-get info :parse-tree)
                  'keyword
                (lambda (keyword)
                  (and (string= (org-element-property :key keyword)
                                org-latex-emoji-keyword)
                       (org-element-property :value keyword)))
                info t)
              (caar org-latex-emoji-sets)))
         (emoji-spec (cdr (assoc emoji-set org-latex-emoji-sets)))
         (emoji-folder
          (expand-file-name emoji-set org-latex-emoji-base-dir))
         (emoji-svg-only
          (and (file-exists-p emoji-folder)
               (not (cl-some
                     (lambda (path)
                       (not (string= (file-name-extension path) "svg")))
                     (directory-files emoji-folder nil "\\....$"))))))
    (cond
     ((not emoji-spec)
      (error "Emoji set `%s' is unknown. Try one of: %s" emoji-set
             (string-join (mapcar #'car org-latex-emoji-sets) ", ")))
     ((not (file-exists-p emoji-folder))
      (if (and (not noninteractive)
               (yes-or-no-p (format "Emoji set `%s' is not installed, would you like to install it?" emoji-set)))
          (org-latex-emoji-install
           emoji-set
           (or (executable-find "cairosvg") (executable-find "inkscape")))
        (error "Emoji set `%s' is not installed" emoji-set))))
    (org-latex-emoji-fill-preamble
     emoji-folder (plist-get emoji-spec :height)
     (plist-get emoji-spec :offset) emoji-svg-only)))

(org-export-update-features 'latex
  (emoji-setup ; The precompilable bit
   :condition (save-excursion
                (goto-char (point-min))
                (re-search-forward org-latex-emoji--rx nil t))
   :requires (image pkg-transparent)
   :snippet org-latex-emoji-setup
   :order 3)
  (pkg-transparent ; Part of emoji setup, but non-precompilable.
   :snippet "\\usepackage{transparent}"
   :order 84)
  (emoji-declarations
   :condition t
   :when emoji-setup
   :snippet
   (mapconcat
    #'org-latex-emoji-declaration
    (let (unicode-cars)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-latex-emoji--rx nil t)
          (push (aref (match-string 0) 0) unicode-cars)))
      (cl-delete-duplicates unicode-cars))
    "\n")
   :order 85))

(org-export-update-features 'latex
  (emoji-lualatex-hack
   :condition t
   :when (emoji julia-code) ; LuaLaTeX is used with julia-code.
   :snippet
   "\\usepackage{newunicodechar}
\\newcommand{\\DeclareUnicodeCharacter}[2]{%
    \\begingroup\\lccode`|=\\string\"#1\\relax
    \\lowercase{\\endgroup\\newunicodechar{|}}{#2}}"
   :before emoji))

(defun org-latex-emoji-install (set &optional convert)
  "Dowload, convert, and install emojis for use with LaTeX."
  (interactive
   (list (completing-read "Emoji set to install: "
                          (mapcar
                           (lambda (set-spec)
                             (if (file-exists-p (expand-file-name (car set-spec) org-latex-emoji-base-dir))
                                 (propertize (car set-spec) 'face 'font-lock-doc-face)
                               (car set-spec)))
                           org-latex-emoji-sets)
                          nil t)
         (if (or (executable-find "cairosvg") (executable-find "inkscape"))
             (yes-or-no-p "Would you like to create .pdf forms of the Emojis (strongly recommended)?")
           (message "Install `cairosvg' (recommended) or `inkscape' to convert to PDF forms")
           nil)))
  (let ((emoji-folder (expand-file-name set org-latex-emoji-base-dir)))
    (when (or (not (file-exists-p emoji-folder))
              (and (not noninteractive)
                   (yes-or-no-p "Emoji folder already present, would you like to re-download?")
                   (progn (delete-directory emoji-folder t) t)))
      (let* ((spec (cdr (assoc set org-latex-emoji-sets)))
             (dir (org-latex-emoji-install--download set (plist-get spec :url)))
             (svg-dir (expand-file-name (or (plist-get spec :folder) "") dir)))
        (org-latex-emoji-install--install
         set svg-dir (plist-get spec :file-regexp))))
    (when convert
      (org-latex-emoji-install--convert (file-name-as-directory emoji-folder))))
  (message "Emojis set `%s' installed." set))

(defun org-latex-emoji-install--download (name url)
  "Download the emoji archive URL for the set NAME."
  (let* ((dest-folder (make-temp-file (format "%s-" name) t)))
    (message "Downloading %s..." name)
    (let ((default-directory dest-folder))
      (call-process "curl" nil nil nil "-sL" url "--output" "emojis.zip")
      (message "Unzipping")
      (call-process "unzip" nil nil nil "emojis.zip")
      dest-folder)))

(defun org-latex-emoji-install--install (name dir &optional filename-regexp)
  "Install the emoji files in DIR to the NAME set folder.
If a FILENAME-REGEXP, only files matching this regexp will be moved,
and they will be renamed to the first capture group of the regexp."
  (message "Installing %s emojis into emoji directory" name)
  (let ((images (append (directory-files dir t ".*.svg")
                        (directory-files dir t ".*.pdf")))
        (emoji-dir (file-name-as-directory
                    (expand-file-name name org-latex-emoji-base-dir))))
    (unless (file-exists-p emoji-dir)
      (make-directory emoji-dir t))
    (mapc
     (lambda (image)
       (if filename-regexp
           (when (string-match filename-regexp (file-name-nondirectory image))
             (rename-file image
                          (expand-file-name
                           (file-name-with-extension
                            (upcase (match-string 1 (file-name-nondirectory image)))
                            (file-name-extension image))
                           emoji-dir)
                          t))
         (rename-file image
                      (expand-file-name
                       (file-name-with-extension
                        (upcase (file-name-nondirectory image))
                        (file-name-extension image))
                       emoji-dir)
                      t)))
     images)
    (message "%d emojis installed" (length images))))

(defun org-latex-emoji-install--convert (dir)
  "Convert all .svg files in DIR to .pdf forms.
Uses cairosvg if possible, falling back to inkscape."
  (let ((default-directory dir))
    (if (executable-find "cairosvg") ; cairo's PDFs are ~10% smaller
        (let* ((images (directory-files dir nil ".*.svg"))
               (num-images (length images))
               (index 0)
               (max-threads (1- (string-to-number (shell-command-to-string "nproc"))))
               (threads 0))
          (while (< index num-images)
            (setf threads (1+ threads))
            (let (message-log-max)
              (message "Converting emoji %d/%d (%s)" (1+ index) num-images (nth index images)))
            (make-process :name "cairosvg"
                          :command (list "cairosvg" (nth index images) "-o" (concat (file-name-sans-extension (nth index images)) ".pdf"))
                          :sentinel (lambda (proc msg)
                                      (when (memq (process-status proc) '(exit signal))
                                        (setf threads (1- threads)))))
            (setq index (1+ index))
            (while (> threads max-threads)
              (sleep-for 0.01)))
          (while (> threads 0)
            (sleep-for 0.01)))
      (message "Cairosvg not found. Proceeding with inkscape as a fallback.")
      (shell-command "inkscape --batch-process --export-type='pdf' *.svg"))
    (message "Finished conversion!")))

(provide 'ox-latex-emoji)
;;; ox-latex-emoji.el ends here
