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
#map { overflow: hidden; height: 80vh; resize: vertical; border: 1px solid #d0d0d0; border-radius: 0.3em; cursor: grab; touch-action: none; }
#map img { width: 100%; display: block; transform-origin: 0 0; user-select: none; -webkit-user-drag: none; }
@media (prefers-color-scheme: dark) {
  body { background: #21242b; color: #bbc2cf; }
  a { color: #51afef; }
  a:visited { color: #c678dd; }
  #map { border-color: #3f444a; }
  img { filter: invert(1) hue-rotate(180deg); }
}
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

<p>Many of these can be thought of as stand-alone packages, with their own
dependencies, some of which interact with Doom modules.</p>

<div id=\"map\"><img src=\"subconf/map.svg\" alt=\"Dependency map of the sub-configuration\" /></div>
<p><small>Scroll to zoom, drag to pan.</small></p>

")
  ;; The dependency map labels each node with its confpkg name, which is nicer
  ;; than the mangled filename and the only record of the original casing.
  (let ((confpkg-names (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert-file-contents "subconf/map.dot")
      (while (re-search-forward "^ +\"\\([^\"]+\\)\" \\[label=\"\\([^\"]+\\)\"\\]$" nil t)
        (puthash (match-string 1) (match-string 2) confpkg-names)))
    (let* ((named (mapcar (lambda (file)
                            (cons file (gethash (file-name-base file) confpkg-names)))
                          (directory-files "subconf" nil "\\.el\\'")))
           (packages (cl-remove-if-not (lambda (e) (string-prefix-p "⚙️ " (or (cdr e) ""))) named))
           (others (cl-remove-if (lambda (e) (memq e packages)) named))
           (insert-section
            (lambda (heading entries)
              (insert (format "\n<h3>%s (%d)</h3>\n\n<ul>" heading (length entries)))
              (pcase-dolist (`(,file . ,name) entries)
                (insert "\n  <li>" (string-remove-prefix "⚙️ " (or name (file-name-base file)))
                        " (<a href=\"engraved/subconf/" file ".html\">pretty</a>, "
                        "<a href=\"subconf/" file "\">raw</a>)"))
              (insert "\n</ul>"))))
      (funcall insert-section "Package configurations" packages)
      (funcall insert-section "Other sub-configuration" others)))
  (insert "\n</div>
<script>
(() => {
  const box = document.getElementById(\"map\"), img = box.firstElementChild;
  let scale = 1, x = 0, y = 0;
  const draw = () =>
    img.style.transform = `translate(${x}px, ${y}px) scale(${scale})`;
  const centre = () => {
    scale = 1;
    x = 0;
    y = (box.clientHeight - img.clientHeight) / 2;
    draw();
  };
  const zoom = (factor, atX, atY) => {
    const previous = scale;
    scale = Math.min(12, Math.max(0.5, scale * factor));
    x = atX - (atX - x) * scale / previous;
    y = atY - (atY - y) * scale / previous;
    draw();
  };
  const local = event => {
    const rect = box.getBoundingClientRect();
    return [event.clientX - rect.left, event.clientY - rect.top];
  };
  if (img.complete) centre(); else img.addEventListener(\"load\", centre);
  new ResizeObserver(centre).observe(box);
  box.addEventListener(\"wheel\", event => {
    event.preventDefault();
    zoom(Math.exp(-event.deltaY / 400), ...local(event));
  }, {passive: false});
  const pointers = new Map();
  let gesture = null;
  const touchpoint = () => {
    const points = [...pointers.values()];
    const mean = axis => points.reduce((sum, p) => sum + p[axis], 0) / points.length;
    return {x: mean(\"x\"), y: mean(\"y\"),
            spread: points.length < 2 ? 0
              : Math.hypot(points[0].x - points[1].x, points[0].y - points[1].y)};
  };
  box.addEventListener(\"pointerdown\", event => {
    event.preventDefault();
    pointers.set(event.pointerId, {x: event.clientX, y: event.clientY});
    box.setPointerCapture(event.pointerId);
    box.style.cursor = \"grabbing\";
    gesture = touchpoint();
  });
  box.addEventListener(\"pointermove\", event => {
    if (!pointers.has(event.pointerId)) return;
    pointers.set(event.pointerId, {x: event.clientX, y: event.clientY});
    const now = touchpoint();
    const rect = box.getBoundingClientRect();
    if (gesture.spread && now.spread)
      zoom(now.spread / gesture.spread,
           gesture.x - rect.left, gesture.y - rect.top);
    x += now.x - gesture.x;
    y += now.y - gesture.y;
    draw();
    gesture = now;
  });
  for (const done of [\"pointerup\", \"pointercancel\"])
    box.addEventListener(done, event => {
      pointers.delete(event.pointerId);
      gesture = pointers.size ? touchpoint() : null;
      if (!gesture) box.style.cursor = \"\";
    });
})();
</script>
</body>\n</html>\n")
  (write-region nil nil "files.html"))

(message "[1;32] Pagelist generated!")

(setq inhibit-message t)
(kill-emacs exit-code)
