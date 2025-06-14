;;; config-ox-html.el --- Generated package (no.82) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.82) from my config.
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

(require 'ox-html)
(require 'config-org-exports)

(define-minor-mode org-fancy-html-export-mode
  "Toggle my fabulous org export tweaks. While this mode itself does a little bit,
the vast majority of the change in behaviour comes from switch statements in:
 - `org-html-template-fancier'
 - `org-html--build-meta-info-extended'
 - `org-html-src-block-collapsable'
 - `org-html-block-collapsable'
 - `org-html-table-wrapped'
 - `org-html--format-toc-headline-colapseable'
 - `org-html--toc-text-stripped-leaves'
 - `org-export-html-headline-anchor'"
  :global t
  :init-value t
  (if org-fancy-html-export-mode
      (setq org-html-style-default org-html-style-fancy
            org-html-meta-tags #'org-html-meta-tags-fancy
            org-html-checkbox-type 'html-span)
    (setq org-html-style-default org-html-style-plain
          org-html-meta-tags #'org-html-meta-tags-default
          org-html-checkbox-type 'html)))

(defadvice! org-html-template-fancier (orig-fn contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options. Adds a few extra things to the body
compared to the default implementation."
  :around #'org-html-template
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn contents info)
    (concat
     (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
       (let* ((xml-declaration (plist-get info :html-xml-declaration))
              (decl (or (and (stringp xml-declaration) xml-declaration)
                        (cdr (assoc (plist-get info :html-extension)
                                    xml-declaration))
                        (cdr (assoc "html" xml-declaration))
                        "")))
         (when (not (or (not decl) (string= "" decl)))
           (format "%s\n"
                   (format decl
                           (or (and org-html-coding-system
                                    (fboundp 'coding-system-get)
                                    (coding-system-get org-html-coding-system 'mime-charset))
                               "iso-8859-1"))))))
     (org-html-doctype info)
     "\n"
     (concat "<html"
             (cond ((org-html-xhtml-p info)
                    (format
                     " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                     (plist-get info :language) (plist-get info :language)))
                   ((org-html-html5-p info)
                    (format " lang=\"%s\"" (plist-get info :language))))
             ">\n")
     "<head>\n"
     (org-html--build-meta-info info)
     (org-html--build-head info)
     (org-html--build-mathjax-config info)
     "</head>\n"
     "<body>\n<input type='checkbox' id='theme-switch'><div id='page'><label id='switch-label' for='theme-switch'></label>"
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)
     ;; Document contents.
     (let ((div (assq 'content (plist-get info :html-divs))))
       (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (and (plist-get info :with-title)
                         (plist-get info :title)))
             (subtitle (plist-get info :subtitle))
             (html5-fancy (org-html--html5-fancy-p info)))
         (when title
           (format
            (if html5-fancy
                "<header class=\"page-header\">%s\n<h1 class=\"title\">%s</h1>\n%s</header>"
              "<h1 class=\"title\">%s%s</h1>\n")
            (if (or (plist-get info :with-date)
                    (plist-get info :with-author))
                (concat "<div class=\"page-meta\">"
                        (when (plist-get info :with-date)
                          (org-export-data (plist-get info :date) info))
                        (when (and (plist-get info :with-date) (plist-get info :with-author)) ", ")
                        (when (plist-get info :with-author)
                          (org-export-data (plist-get info :author) info))
                        "</div>\n")
              "")
            (org-export-data title info)
            (if subtitle
                (format
                 (if html5-fancy
                     "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                   (concat "\n" (org-html-close-tag "br" nil info) "\n"
                           "<span class=\"subtitle\">%s</span>\n"))
                 (org-export-data subtitle info))
              "")))))
     contents
     (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Possibly use the Klipse library live code blocks.
     (when (plist-get info :html-klipsify-src)
       (concat "<script>" (plist-get info :html-klipse-selection-script)
               "</script><script src=\""
               org-html-klipse-js
               "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
               org-html-klipse-css "\"/>"))
     ;; Closing document.
     "</div>\n</body>\n</html>")))

(defadvice! org-html-toc-linked (depth info &optional scope)
  "Build a table of contents.

Just like `org-html-toc', except the header is a link to \"#\".

DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  :override #'org-html-toc
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\">"
                         (org-html--toc-text toc-entries)
                         "</div>\n")))
        (if scope toc
          (let ((outer-tag (if (org-html--html5-fancy-p info)
                               "nav"
                             "div")))
            (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
                    (let ((top-level (plist-get info :html-toplevel-hlevel)))
                      (format "<h%d><a href=\"#\" style=\"color:inherit; text-decoration: none;\">%s</a></h%d>\n"
                              top-level
                              (org-html--translate "Table of Contents" info)
                              top-level))
                    toc
                    (format "</%s>\n" outer-tag))))))))

(defvar org-html-meta-tags-opengraph-image
  '(:image "https://tecosaur.com/resources/org/nib.png"
    :type "image/png"
    :width "200"
    :height "200"
    :alt "Green fountain pen nib")
  "Plist of og:image:PROP properties and their value, for use in `org-html-meta-tags-fancy'.")

(defun org-html-meta-tags-fancy (info)
  "Use the INFO plist to construct the meta tags, as described in `org-html-meta-tags'."
  (let* ((title (org-html-plain-text
                 (org-element-interpret-data (plist-get info :title)) info))
         (author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        ;; Return raw Org syntax.
                        (and auth (org-html-plain-text
                                   (org-element-interpret-data auth) info)))))
         (author-first-last
          (and (not (org-string-nw-p author))
               (save-match-data
                 (if (string-match "\\`\\(.+?\\) +\\(.+?\\)\\'" author)
                     (cons (match-string 1 author)
                           (match-string 2 author))
                   (cons author nil))))))
    (append
     (list
      (when (org-string-nw-p author)
        (list "name" "author" author))
      (when (org-string-nw-p (plist-get info :description))
        (list "name" "description"
              (plist-get info :description)))
      '("name" "generator" "org mode")
      '("name" "theme-color" "#77aa99")
      '("property" "og:type" "article")
      (list "property" "og:title" title)
      (let ((subtitle (org-export-data (plist-get info :subtitle) info)))
        (when (org-string-nw-p subtitle)
          (list "property" "og:description" subtitle))))
     (when org-html-meta-tags-opengraph-image
       (list (list "property" "og:image" (plist-get org-html-meta-tags-opengraph-image :image))
             (list "property" "og:image:type" (plist-get org-html-meta-tags-opengraph-image :type))
             (list "property" "og:image:width" (plist-get org-html-meta-tags-opengraph-image :width))
             (list "property" "og:image:height" (plist-get org-html-meta-tags-opengraph-image :height))
             (list "property" "og:image:alt" (plist-get org-html-meta-tags-opengraph-image :alt))))
     (list
      (when (car author-first-last)
        (list "property" "og:article:author:first_name" (car author-first-last)))
      (when (cdr author-first-last)
        (list "property" "og:article:author:last_name" (cdr author-first-last)))
      (list "property" "og:article:published_time"
            (format-time-string
             "%FT%T%z"
             (or
              (when-let ((date-str (cadar (org-collect-keywords '("DATE")))))
                (unless (string= date-str (format-time-string "%F"))
                  (ignore-errors (encode-time (org-parse-time-string date-str)))))
              (if buffer-file-name
                  (file-attribute-modification-time (file-attributes buffer-file-name))
                (current-time)))))
      (when buffer-file-name
        (list "property" "og:article:modified_time"
              (format-time-string "%FT%T%z" (file-attribute-modification-time (file-attributes buffer-file-name)))))))))

(unless (functionp #'org-html-meta-tags-default)
  (defalias 'org-html-meta-tags-default #'ignore))
(setq org-html-meta-tags #'org-html-meta-tags-fancy)

(setq org-html-style-plain org-html-style-default
      org-html-htmlize-output-type 'css
      org-html-doctype "html5"
      org-html-html5-fancy t)

(defun org-html-reload-fancy-style ()
  (interactive)
  (setq org-html-style-fancy
        (with-temp-buffer
          (insert-file-contents (expand-file-name "misc/org-export-header.html" doom-user-dir))
          (goto-char (point-max))
          (insert "<script>\n")
          (insert-file-contents (expand-file-name "misc/org-css/main.js" doom-user-dir))
          (goto-char (point-max))
          (insert "</script>\n<style>\n")
          (insert-file-contents (expand-file-name "misc/org-css/main.min.css" doom-user-dir))
          (goto-char (point-max))
          (insert "</style>")
          (buffer-string)))
  (when org-fancy-html-export-mode
    (setq org-html-style-default org-html-style-fancy)))
(org-html-reload-fancy-style)

(defvar org-html-export-collapsed nil)
(eval '(cl-pushnew '(:collapsed "COLLAPSED" "collapsed" org-html-export-collapsed t)
                   (org-export-backend-options (org-export-get-backend 'html))))
(add-to-list 'org-default-properties "EXPORT_COLLAPSED")

(defadvice! org-html-src-block-collapsable (orig-fn src-block contents info)
  "Wrap the usual <pre> block in a <details>"
  :around #'org-html-src-block
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn src-block contents info)
    (let* ((properties (cadr src-block))
           (lang (mode-name-to-lang-name
                  (plist-get properties :language)))
           (name (plist-get properties :name))
           (ref (org-export-get-reference src-block info))
           (collapsed-p (member (or (org-export-read-attribute :attr_html src-block :collapsed)
                                    (plist-get info :collapsed))
                                '("y" "yes" "t" t "true" "all"))))
      (format
       "<details id='%s' class='code'%s><summary%s>%s</summary>
<div class='gutter'>
<a href='#%s'>#</a>
<button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
</div>
%s
</details>"
       ref
       (if collapsed-p "" " open")
       (if name " class='named'" "")
       (concat
        (when name (concat "<span class=\"name\">" name "</span>"))
        "<span class=\"lang\">" lang "</span>")
       ref
       (if name
           (replace-regexp-in-string (format "<pre\\( class=\"[^\"]+\"\\)? id=\"%s\">" ref) "<pre\\1>"
                                     (funcall orig-fn src-block contents info))
         (funcall orig-fn src-block contents info))))))

(defun mode-name-to-lang-name (mode)
  (or (cadr (assoc mode
                   '(("asymptote" "Asymptote")
                     ("awk" "Awk")
                     ("C" "C")
                     ("clojure" "Clojure")
                     ("css" "CSS")
                     ("D" "D")
                     ("ditaa" "ditaa")
                     ("dot" "Graphviz")
                     ("calc" "Emacs Calc")
                     ("emacs-lisp" "Emacs Lisp")
                     ("fortran" "Fortran")
                     ("gnuplot" "gnuplot")
                     ("haskell" "Haskell")
                     ("hledger" "hledger")
                     ("java" "Java")
                     ("js" "Javascript")
                     ("latex" "LaTeX")
                     ("ledger" "Ledger")
                     ("lisp" "Lisp")
                     ("lilypond" "Lilypond")
                     ("lua" "Lua")
                     ("matlab" "MATLAB")
                     ("mscgen" "Mscgen")
                     ("ocaml" "Objective Caml")
                     ("octave" "Octave")
                     ("org" "Org mode")
                     ("oz" "OZ")
                     ("plantuml" "Plantuml")
                     ("processing" "Processing.js")
                     ("python" "Python")
                     ("R" "R")
                     ("ruby" "Ruby")
                     ("sass" "Sass")
                     ("scheme" "Scheme")
                     ("screen" "Gnu Screen")
                     ("sed" "Sed")
                     ("sh" "shell")
                     ("sql" "SQL")
                     ("sqlite" "SQLite")
                     ("forth" "Forth")
                     ("io" "IO")
                     ("J" "J")
                     ("makefile" "Makefile")
                     ("maxima" "Maxima")
                     ("perl" "Perl")
                     ("picolisp" "Pico Lisp")
                     ("scala" "Scala")
                     ("shell" "Shell Script")
                     ("ebnf2ps" "ebfn2ps")
                     ("cpp" "C++")
                     ("abc" "ABC")
                     ("coq" "Coq")
                     ("groovy" "Groovy")
                     ("bash" "bash")
                     ("csh" "csh")
                     ("ash" "ash")
                     ("dash" "dash")
                     ("ksh" "ksh")
                     ("mksh" "mksh")
                     ("posh" "posh")
                     ("ada" "Ada")
                     ("asm" "Assembler")
                     ("caml" "Caml")
                     ("delphi" "Delphi")
                     ("html" "HTML")
                     ("idl" "IDL")
                     ("mercury" "Mercury")
                     ("metapost" "MetaPost")
                     ("modula-2" "Modula-2")
                     ("pascal" "Pascal")
                     ("ps" "PostScript")
                     ("prolog" "Prolog")
                     ("simula" "Simula")
                     ("tcl" "tcl")
                     ("tex" "LaTeX")
                     ("plain-tex" "TeX")
                     ("verilog" "Verilog")
                     ("vhdl" "VHDL")
                     ("xml" "XML")
                     ("nxml" "XML")
                     ("conf" "Configuration File"))))
      mode))

(defun org-html-block-collapsable (orig-fn block contents info)
  "Wrap the usual block in a <details>"
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn block contents info)
    (let ((ref (org-export-get-reference block info))
          (type (pcase (car block)
                  ('property-drawer "Properties")))
          (collapsed-default (pcase (car block)
                               ('property-drawer t)
                               (_ nil)))
          (collapsed-value (org-export-read-attribute :attr_html block :collapsed))
          (collapsed-p (or (member (org-export-read-attribute :attr_html block :collapsed)
                                   '("y" "yes" "t" t "true"))
                           (member (plist-get info :collapsed) '("all")))))
      (format
       "<details id='%s' class='code'%s>
<summary%s>%s</summary>
<div class='gutter'>\
<a href='#%s'>#</a>
<button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
</div>
%s\n
</details>"
       ref
       (if (or collapsed-p collapsed-default) "" " open")
       (if type " class='named'" "")
       (if type (format "<span class='type'>%s</span>" type) "")
       ref
       (funcall orig-fn block contents info)))))

(advice-add 'org-html-example-block   :around #'org-html-block-collapsable)
(advice-add 'org-html-fixed-width     :around #'org-html-block-collapsable)
(advice-add 'org-html-property-drawer :around #'org-html-block-collapsable)

(autoload #'highlight-numbers--turn-on "highlight-numbers")
(add-hook 'htmlize-before-hook #'highlight-numbers--turn-on)

(defadvice! org-html-table-wrapped (orig-fn table contents info)
  "Wrap the usual <table> in a <div>"
  :around #'org-html-table
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn table contents info)
    (let* ((name (plist-get (cadr table) :name))
           (ref (org-export-get-reference table info)))
      (format "<div id='%s' class='table'>
<div class='gutter'><a href='#%s'>#</a></div>
<div class='tabular'>
%s
</div>\
</div>"
              ref ref
              (if name
                  (replace-regexp-in-string (format "<table id=\"%s\"" ref) "<table"
                                            (funcall orig-fn table contents info))
                (funcall orig-fn table contents info))))))

(defadvice! org-html--format-toc-headline-colapseable (orig-fn headline info)
  "Add a label and checkbox to `org-html--format-toc-headline's usual output,
to allow the TOC to be a collapseable tree."
  :around #'org-html--format-toc-headline
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn headline info)
    (let ((id (or (org-element-property :CUSTOM_ID headline)
                  (org-export-get-reference headline info))))
      (format "<input type='checkbox' id='toc--%s'/><label for='toc--%s'>%s</label>"
              id id (funcall orig-fn headline info)))))

(defadvice! org-html--toc-text-stripped-leaves (orig-fn toc-entries)
  "Remove label"
  :around #'org-html--toc-text
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn toc-entries)
    (replace-regexp-in-string "<input [^>]+><label [^>]+>\\(.+?\\)</label></li>" "\\1</li>"
                              (funcall orig-fn toc-entries))))

(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<kbd>%s</kbd>")))

(appendq! org-html-checkbox-types
          '((html-span
             (on . "<span class='checkbox'></span>")
             (off . "<span class='checkbox'></span>")
             (trans . "<span class='checkbox'></span>"))))
(setq org-html-checkbox-type 'html-span)

(pushnew! org-html-special-string-regexps
          '("-&gt;" . "&#8594;")
          '("&lt;-" . "&#8592;"))

(defun org-export-html-headline-anchor (text backend info)
  (when (and (org-export-derived-backend-p backend 'html)
             (not (org-export-derived-backend-p backend 're-reveal))
             org-fancy-html-export-mode)
    (unless (bound-and-true-p org-msg-export-in-progress)
      (replace-regexp-in-string
       "<h\\([0-9]\\) id=\"\\([a-z0-9-]+\\)\">\\(.*[^ ]\\)<\\/h[0-9]>" ; this is quite restrictive, but due to `org-reference-contraction' I can do this
       "<h\\1 id=\"\\2\">\\3<a aria-hidden=\"true\" href=\"#\\2\">#</a> </h\\1>"
       text))))

(add-to-list 'org-export-filter-headline-functions
             'org-export-html-headline-anchor)

(org-link-set-parameters "Https"
                         :follow (lambda (url arg) (browse-url (concat "https:" url) arg))
                         :export #'org-url-fancy-export)

(defun org-url-fancy-export (url _desc backend)
  (let ((metadata (org-url-unfurl-metadata (concat "https:" url))))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (concat
       "<div class=\"link-preview\">"
       (format "<a href=\"%s\">" (concat "https:" url))
       (when (plist-get metadata :image)
         (format "<img src=\"%s\"/>" (plist-get metadata :image)))
       "<small>"
       (replace-regexp-in-string "//\\(?:www\\.\\)?\\([^/]+\\)/?.*" "\\1" url)
       "</small><p>"
       (when (plist-get metadata :title)
         (concat "<b>" (org-html-encode-plain-text (plist-get metadata :title)) "</b></br>"))
       (when (plist-get metadata :description)
         (org-html-encode-plain-text (plist-get metadata :description)))
       "</p></a></div>"))
     (t url))))

(setq org-url-unfurl-metadata--cache nil)
(defun org-url-unfurl-metadata (url)
  (cdr (or (assoc url org-url-unfurl-metadata--cache)
           (car (push
                 (cons
                  url
                  (let* ((head-data
                          (cl-remove-if-not
                           #'listp
                           (cdaddr
                            (with-current-buffer
                                (progn (message "Fetching metadata from %s" url)
                                       (if (executable-find "curl")
                                           (with-current-buffer (generate-new-buffer " *curl*")
                                             (call-process "curl" nil t nil "--max-time" "5" "-sSL" url)
                                             (current-buffer))
                                         (url-retrieve-synchronously url t t 5)))
                              (goto-char (point-min))
                              (delete-region (point-min) (- (search-forward "<head") 6))
                              (delete-region (search-forward "</head>") (point-max))
                              (goto-char (point-min))
                              (while (re-search-forward "<script[^\u2800]+?</script>" nil t)
                                (replace-match ""))
                              (goto-char (point-min))
                              (while (re-search-forward "<style[^\u2800]+?</style>" nil t)
                                (replace-match ""))
                              (libxml-parse-html-region (point-min) (point-max))))))
                         (meta (delq nil
                                     (mapcar
                                      (lambda (tag)
                                        (when (eq 'meta (car tag))
                                          (cons (or (cdr (assoc 'name (cadr tag)))
                                                    (cdr (assoc 'property (cadr tag))))
                                                (cdr (assoc 'content (cadr tag))))))
                                      head-data))))
                    (let ((title (or (cdr (assoc "og:title" meta))
                                     (cdr (assoc "twitter:title" meta))
                                     (nth 2 (assq 'title head-data))))
                          (description (or (cdr (assoc "og:description" meta))
                                           (cdr (assoc "twitter:description" meta))
                                           (cdr (assoc "description" meta))))
                          (image (or (cdr (assoc "og:image" meta))
                                     (cdr (assoc "twitter:image" meta)))))
                      (when image
                        (setq image (replace-regexp-in-string
                                     "^/" (concat "https://" (replace-regexp-in-string "//\\([^/]+\\)/?.*" "\\1" url) "/")
                                     (replace-regexp-in-string
                                      "^//" "https://"
                                      image))))
                      (list :title title :description description :image image))))
                 org-url-unfurl-metadata--cache)))))

;; (setq-default org-html-with-latex `dvisvgm)

(setcdr (assoc 'path org-html-mathjax-options)
        (list "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"))

(setq org-html-mathjax-template
  "<script>
  window.MathJax = {
    loader: {
      load: ['[tex]/mathtools'],
    },
    tex: {
      ams: {
        multlineWidth: '%MULTLINEWIDTH'
      },
      tags: '%TAGS',
      tagSide: '%TAGSIDE',
      tagIndent: '%TAGINDENT',
      packages: {'[+]': ['mathtools']},
      macros: {
        RR: ['\\\\ifstrempty{#1}{\\\\mathbb{R}}{\\\\mathbb{R}^{#1}}', 1, ''],
        NN: ['\\\\ifstrempty{#1}{\\\\mathbb{N}}{\\\\mathbb{N}^{#1}}', 1, ''],
        ZZ: ['\\\\ifstrempty{#1}{\\\\mathbb{Z}}{\\\\mathbb{Z}^{#1}}', 1, ''],
        QQ: ['\\\\ifstrempty{#1}{\\\\mathbb{Q}}{\\\\mathbb{Q}^{#1}}', 1, ''],
        CC: ['\\\\ifstrempty{#1}{\\\\mathbb{C}}{\\\\mathbb{C}^{#1}}', 1, ''],
        EE: '\\\\mathbb{E}',
        Lap: '\\\\operatorname{\\\\mathcal{L}}',
        Var: '\\\\operatorname{Var}',
        Cor: '\\\\operatorname{Cor}',
        E: '\\\\operatorname{E}',
      },
      mathtools: {
        pairedDelimiters: {
          abs: ['\\\\lvert', '\\\\rvert'],
          norm: ['\\\\lVert', '\\\\rVert'],
          ceil: ['\\\\lceil', '\\\\rceil'],
          floor: ['\\\\lfloor', '\\\\rfloor'],
          round: ['\\\\lfloor', '\\\\rceil'],
        }
      }
    },
    chtml: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    svg: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    output: {
      font: '%FONT',
      displayOverflow: '%OVERFLOW'
    }
  };
</script>

<script
  id=\"MathJax-script\"
  async
  src=\"%PATH\">
</script>")

(provide 'config-ox-html)
;;; config-ox-html.el ends here
