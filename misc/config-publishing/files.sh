#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (expand-file-name (format "%s-log.txt" (file-name-base load-file-name))))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise 'light)

;;; Actually do the exporting now

(message "[34] Creating pagelist")

(setq default-directory publish-dir)

(with-temp-buffer
  (insert "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\" />
<title>Doom Emacs Configuration: Files</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
<meta name=\"theme-color\" content=\"#77aa99\" />
<style>
body { background: #fafafa; color: #383a42; margin: 0; padding: 1em; }
body > div { max-width: 60rem; margin: auto; }
p, li { font-size: 1.1em }
code { font-size: 1.2em; }
a { color: #4078f2; }
a:visited { color: #b751b6; }
</style>
</head>
<body>
<div>

<h1>Doom Emacs Configuration &mdash; Files</h1>

<p><a href=\"engraved/config.org.html\"><code>config.org</code></a> (<a href=\"config.html\">html</a>, <a href=\"config.pdf\">pdf</a>)</p>

<p>Upon <a href=\"https://en.wikipedia.org/wiki/Literate_programming\">tangling</a> this file,
every other file listed here is generated.</p>

<p>Doom cares about three in particular:</p>
<ul>
  <li><a href=\"engraved/init.el.html\"><code>init.el</code></a></li>
  <li><a href=\"engraved/packages.el.html\"><code>packages.el</code></a></li>
  <li><a href=\"engraved/config.el.html\"><code>config.el</code></a></li>
</ul>

<h2>Generated sub-configuration</h2>

<p>The sub-configuration is a collection of generated fragments of configuration sourced from <code>config.org</code>.
See <a href=\"config.html#rudimentary-configuration-confpkg\">the section on it</a> for more information.</p>

<p>Many of these can be thought of as stand-alone packages</a>

<ul>")
  (dolist (subconf (directory-files "subconf"))
    (unless (member subconf '("." ".."))
      (insert "\n  <li><a href=\"engraved/subconf/" subconf
              ".html\"><code>" subconf "</code></a> (<a href=\"subconf/"
              subconf "\">raw</a>)")))
  (insert "\n</ul>\n</div>\n</body>\n</html>\n")
  (write-region nil nil "files.html"))

(message "[1;32] Pagelist generated!")

(setq inhibit-message t)
(kill-emacs exit-code)
