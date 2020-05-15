;;; org-plot.el --- Support for Plotting from Org -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.
;;
;; Author: Eric Schulte <schulte dot eric at gmail dot com>
;; Keywords: tables, plotting
;; Homepage: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Borrows ideas and a couple of lines of code from org-exp.el.

;; Thanks to the Org mailing list for testing and implementation and
;; feature suggestions

;; This has been modified by @tecosaur to extend the functionality of this package

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-table)

(declare-function gnuplot-delchar-or-maybe-eof "ext:gnuplot" (arg))
(declare-function gnuplot-mode "ext:gnuplot" ())
(declare-function gnuplot-send-buffer-to-gnuplot "ext:gnuplot" ())

(defvar org-plot/gnuplot-default-options
  '((:plot-type . 2d)
    (:with . lines)
    (:ind . 0))
  "Default options to gnuplot used by `org-plot/gnuplot'.")

(defvar org-plot-timestamp-fmt nil)

(defun org-plot/add-options-to-plist (p options)
  "Parse an OPTIONS line and set values in the property list P.
Returns the resulting property list."
  (when options
    (let ((op '(("type"    . :plot-type)
                ("script"  . :script)
                ("line"    . :line)
                ("set"     . :set)
                ("title"   . :title)
                ("ind"     . :ind)
                ("deps"    . :deps)
                ("with"    . :with)
                ("file"    . :file)
                ("labels"  . :labels)
                ("map"     . :map)
                ("timeind" . :timeind)
                ("timefmt" . :timefmt)
                ;; extra from me
                ("min"       . :ymin)
                ("max"       . :ymax)
                ("ymin"      . :ymin)
                ("ymax"      . :ymax)
                ("ticks"     . :ticks)
                ("t"         . :transpose)
                ("transpose" . :transpose)))
          (multiples '("set" "line"))
          (regexp ":\\([\"][^\"]+?[\"]\\|[(][^)]+?[)]\\|[^ \t\n\r;,.]*\\)")
          (start 0))
      (dolist (o op)
        (if (member (car o) multiples) ;; keys with multiple values
            (while (string-match
                    (concat (regexp-quote (car o)) regexp)
                    options start)
              (setq start (match-end 0))
              (setq p (plist-put p (cdr o)
                                 (cons (car (read-from-string
                                             (match-string 1 options)))
                                       (plist-get p (cdr o)))))
              p)
          (if (string-match (concat (regexp-quote (car o)) regexp)
                            options)
              (setq p (plist-put p (cdr o)
                                 (car (read-from-string
                                       (match-string 1 options))))))))))
  p)

(defun org-plot/goto-nearest-table ()
  "Move the point forward to the beginning of nearest table.
Return value is the point at the beginning of the table."
  (interactive) (move-beginning-of-line 1)
  (while (not (or (org-at-table-p) (< 0 (forward-line 1)))))
  (goto-char (org-table-begin)))

(defun org-plot/collect-options (&optional params)
  "Collect options from an org-plot `#+Plot:' line.
Accepts an optional property list PARAMS, to which the options
will be added.  Returns the resulting property list."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (string-match "#\\+PLOT: +\\(.*\\)$" line)
        (org-plot/add-options-to-plist params (match-string 1 line))
      params)))

(defun org-plot-quote-timestamp-field (s)
  "Convert field S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-plot-timestamp-fmt (org-time-string-to-time s)))

(defun org-plot-quote-tsv-field (s)
  "Quote field S for export to gnuplot."
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
        (org-plot-quote-timestamp-field s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun org-plot/gnuplot-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (with-temp-file
      data-file
    (setq-local org-plot-timestamp-fmt (or
                                        (plist-get params :timefmt)
                                        "%Y-%m-%d-%H:%M:%S"))
    (insert (orgtbl-to-generic
             table
             (org-combine-plists
              '(:sep "\t" :fmt org-plot-quote-tsv-field)
              params))))
  nil)

(defun org-plot/gnuplot-to-grid-data (table data-file params)
  "Export the data in TABLE to DATA-FILE for gnuplot.
This means in a format appropriate for grid plotting by gnuplot.
PARAMS specifies which columns of TABLE should be plotted as independent
and dependent variables."
  (interactive)
  (let* ((ind (- (plist-get params :ind) 1))
         (deps (if (plist-member params :deps)
                   (mapcar (lambda (val) (- val 1)) (plist-get params :deps))
                 (let (collector)
                   (dotimes (col (length (nth 0 table)))
                     (setf collector (cons col collector)))
                   collector)))
         (counter 0)
         row-vals)
    (when (>= ind 0) ;; collect values of ind col
      (setf row-vals (mapcar (lambda (row) (setf counter (+ 1 counter))
                               (cons counter (nth ind row)))
                             table)))
    (when (or deps (>= ind 0)) ;; remove non-plotting columns
      (setf deps (delq ind deps))
      (setf table (mapcar (lambda (row)
                            (dotimes (col (length row))
                              (unless (memq col deps)
                                (setf (nth col row) nil)))
                            (delq nil row))
                          table)))
    ;; write table to gnuplot grid datafile format
    (with-temp-file data-file
      (let ((num-rows (length table)) (num-cols (length (nth 0 table)))
            (gnuplot-row (lambda (col row value)
                           (setf col (+ 1 col)) (setf row (+ 1 row))
                           (format "%f  %f  %f\n%f  %f  %f\n"
                                   col (- row 0.5) value ;; lower edge
                                   col (+ row 0.5) value))) ;; upper edge
            front-edge back-edge)
        (dotimes (col num-cols)
          (dotimes (row num-rows)
            (setf back-edge
                  (concat back-edge
                          (funcall gnuplot-row (- col 1) row
                                   (string-to-number (nth col (nth row table))))))
            (setf front-edge
                  (concat front-edge
                          (funcall gnuplot-row col row
                                   (string-to-number (nth col (nth row table)))))))
          ;; only insert once per row
          (insert back-edge) (insert "\n") ;; back edge
          (insert front-edge) (insert "\n") ;; front edge
          (setf back-edge "") (setf front-edge ""))))
    row-vals))

(defun org--plot/gnuplot-script-preamble ()
  "Generate a preamble to be prepended to the script run in `org-plot/gnuplot-script'"
  (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set linetype -1 lc rgb '%s'
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
          (doom-color 'violet)
          ))

(defun org-plot/gnuplot-script (table data-file num-cols params &optional preface)
  "Write a gnuplot script to DATA-FILE respecting the options set in PARAMS.
NUM-COLS controls the number of columns plotted in a 2-d plot.
Optional argument PREFACE returns only option parameters in a
manner suitable for prepending to a user-specified script."
  (let* ((type (plist-get params :plot-type))
         (with (if (eq type 'grid) 'pm3d (plist-get params :with)))
         (sets (plist-get params :set))
         (lines (plist-get params :line))
         (map (plist-get params :map))
         (title (plist-get params :title))
         (file (plist-get params :file))
         (ind (plist-get params :ind))
         (time-ind (plist-get params :timeind))
         (timefmt (plist-get params :timefmt))
         (text-ind (plist-get params :textind))
         (deps (if (plist-member params :deps) (plist-get params :deps)))
         (col-labels (plist-get params :labels))
         (x-labels (plist-get params :xlabels))
         (y-labels (plist-get params :ylabels))
         (plot-str "'%s' using %s%d%s with %s title '%s'")
         (plot-cmd (pcase type
                     (`2d "plot")
                     (`3d "splot")
                     (`grid "splot")))
         (script "reset")
         ;; ats = add-to-script
         (ats (lambda (line) (setf script (concat script "\n" line))))
         plot-lines)

    ;; handle output file
    (funcall ats (format "set term %s background rgb '%s' size 1050,650"
                         (if file (file-name-extension file) "GNUTERM")
                         (doom-color 'bg)))
    (when file (funcall ats (format "set output '%s'" file)))

    (funcall ats "# HELLO")

    (funcall ats (org--plot/gnuplot-script-preamble))

    (pcase type				; type
      (`2d ())
      (`3d (when map (funcall ats "set map")))
      (`grid (funcall ats (if map "set pm3d map" "set pm3d"))))
    (when title (funcall ats (format "set title '%s'" title))) ; title
    (mapc ats lines)					       ; line
    (dolist (el sets) (funcall ats (format "set %s" el)))      ; set
    ;; Unless specified otherwise, values are TAB separated.
    (unless (string-match-p "^set datafile separator" script)
      (funcall ats "set datafile separator \"\\t\""))
    (when x-labels			; x labels (xtics)
      (funcall ats
               (format "set xtics (%s)"
                       (mapconcat (lambda (pair)
                                    (format "\"%s\" %d" (cdr pair) (car pair)))
                                  x-labels ", "))))
    (when y-labels			; y labels (ytics)
      (funcall ats
               (format "set ytics (%s)"
                       (mapconcat (lambda (pair)
                                    (format "\"%s\" %d" (cdr pair) (car pair)))
                                  y-labels ", "))))
    (when time-ind			; timestamp index
      (funcall ats "set xdata time")
      (funcall ats (concat "set timefmt \""
                           (or timefmt	; timefmt passed to gnuplot
                               "%Y-%m-%d-%H:%M:%S") "\"")))
    (unless preface
      (pcase type			; plot command
        (`2d (dotimes (col num-cols)
               (unless (and (eq type '2d)
                            (or (and ind (equal (1+ col) ind))
                                (and deps (not (member (1+ col) deps)))))
                 (setf plot-lines
                       (cons
                        (format plot-str data-file
                                (or (and ind (> ind 0)
                                         (not text-ind)
                                         (format "%d:" ind)) "")
                                (1+ col)
                                (if text-ind (format ":xticlabel(%d)" ind) "")
                                with
                                (or (nth col col-labels)
                                    (format "%d" (1+ col))))
                        plot-lines)))))
        (`3d
         (setq plot-lines (list (format "'%s' matrix with %s title ''"
                                        data-file with))))
        (`grid
         (setq plot-lines (list (format "'%s' with %s title ''"
                                        data-file with))))
        (`radar
         (setq plot-lines (list (org--plot/radar table params)))))
      (funcall ats
               (concat plot-cmd " " (mapconcat #'identity
                                               (reverse plot-lines)
                                               ",\\\n    "))))
    script))

(defvar org--plot/radar-template
  "### spider plot/chart with gnuplot
# also known as: radar chart, web chart, star chart, cobweb chart,
#                radar plot,  web plot,  star plot,  cobweb plot,  etc. ...
set datafile separator ' '
set size square
unset tics
set angles degree
set key bmargin center horizontal
unset border

# Load data and settup
load \"%s\"

# General settings
DataColCount = words($Data[1])-1
AxesCount = |$Data|-HeaderLines
AngleOffset = 90
Max = 1
d=0.1*Max
Direction = -1   # counterclockwise=1, clockwise = -1

# Tic settings
TicCount = %s
TicOffset = 0.1
TicValue(axis,i) = real(i)*(word($Settings[axis],3)-word($Settings[axis],2)) \\
          / word($Settings[axis],4)+word($Settings[axis],2)
TicLabelPosX(axis,i) = PosX(axis,i/TicCount) + PosY(axis, TicOffset)
TicLabelPosY(axis,i) = PosY(axis,i/TicCount) - PosX(axis, TicOffset)
TicLen = 0.03
TicdX(axis,i) = 0.5*TicLen*cos(alpha(axis)-90)
TicdY(axis,i) = 0.5*TicLen*sin(alpha(axis)-90)

# Label
LabOffset = 0.10
LabX(axis) = PosX(axis+1,Max+2*d) + PosY(axis, LabOffset)
LabY(axis) = PosY($0+1,Max+2*d)

# Functions
alpha(axis) = (axis-1)*Direction*360.0/AxesCount+AngleOffset
PosX(axis,R) = R*cos(alpha(axis))
PosY(axis,R) = R*sin(alpha(axis))
Scale(axis,value) = real(value-word($Settings[axis],2))/(word($Settings[axis],3)-word($Settings[axis],2))

# Spider settings
set style arrow 1 dt 1 lw 1.0 @fgal head filled size 0.06,25     # style for axes
set style arrow 2 dt 2 lw 0.5 @fgal nohead   # style for weblines
set style arrow 3 dt 1 lw 1 @fgal nohead     # style for axis tics
set samples AxesCount
set isosamples TicCount
set urange[1:AxesCount]
set vrange[1:TicCount]
set style fill transparent solid 0.2

set xrange[-Max-4*d:Max+4*d]
set yrange[-Max-4*d:Max+4*d]
plot \\
    '+' u (0):(0):(PosX($0,Max+d)):(PosY($0,Max+d)) w vec as 1 not, \\
    $Data u (LabX($0)): \\
        (LabY($0)):1 every ::HeaderLines w labels center enhanced @fgt not, \\
    for [i=1:DataColCount] $Data u (PosX($0+1,Scale($0+1,column(i+1)))): \\
        (PosY($0+1,Scale($0+1,column(i+1)))) every ::HeaderLines w filledcurves lt i title word($Data[1],i+1), \\
%s
#    '++' u (PosX($1,$2/TicCount)-TicdX($1,$2/TicCount)): \\
#        (PosY($1,$2/TicCount)-TicdY($1,$2/TicCount)): \\
#        (2*TicdX($1,$2/TicCount)):(2*TicdY($1,$2/TicCount)) \\
#        w vec as 3 not, \\
### end of code
")

(defvar org--plot/radar-ticks
  "    '++' u (PosX($1,$2/TicCount)):(PosY($1,$2/TicCount)): \\
        (PosX($1+1,$2/TicCount)-PosX($1,$2/TicCount)):  \\
        (PosY($1+1,$2/TicCount)-PosY($1,$2/TicCount)) w vec as 2 not, \\
    '++' u (TicLabelPosX(%s,$2)):(TicLabelPosY(%s,$2)): \\
        (sprintf('%%g',TicValue(%s,$2))) w labels font ',8' @fgat not")

(defvar org--plot/radar-setup-template
  "# Data
$Data <<HEREHAVESOMEDATA
%s
HEREHAVESOMEDATA
HeaderLines = 1

# Settings for scale and offset adjustments
# axis min max tics axisLabelXoff axisLabelYoff
$Settings <<EOD
%s
EOD
")

(defun org--plot/radar (table params)
  (let* ((data
          (concat "\"" (s-join "\" \"" (plist-get params :labels)) "\""
                  "\n"
                  (s-join "\n"
                          (mapcar (lambda (row)
                                    (format
                                     "\"%s\" %s"
                                     (car row)
                                     (s-join " " (cdr row))))
                                  table))))
         (ticks (or (plist-get params :ticks)
                    (org--plot/sensible-tick-num table
                                                 (plist-get params :ymin)
                                                 (plist-get params :ymax))))
         (settings
          (s-join "\n"
                  (mapcar (lambda (row)
                            (let ((data (org--plot/values-stats
                                         (mapcar #'string-to-number (cdr row)))))
                              (format
                               "\"%s\" %s %s %s"
                               (car row)
                               (or (plist-get params :ymin)
                                   (plist-get data :nice-min))
                               (or (plist-get params :ymax)
                                   (plist-get data :nice-max))
                               (if (eq ticks 0) 2 ticks)
                               )))
                          table)))
         (setup-file (make-temp-file "org-plot-setup")))
    (f-write-text (format org--plot/radar-setup-template data settings)
                  'utf-8 setup-file)
    (format org--plot/radar-template
            setup-file
            (if (eq ticks 0) 2 ticks)
            (if (eq ticks 0) ""
              (apply #'format org--plot/radar-ticks
                     (make-list 3 (if (and (plist-get params :ymin)
                                           (plist-get params :ymax))
                                      ;; FIXME multi-drawing of tick labels with "1"
                                      "1" "$1")))))))

(defun org--plot/values-stats (nums &optional hard-min hard-max)
  (let* ((minimum (or hard-min (apply #'min nums)))
         (maximum (or hard-max (apply #'max nums)))
         (range (- maximum minimum))
         (rangeOrder (ceiling (- 1 (log10 range))))
         (range-factor (expt 10 rangeOrder))
         (nice-min (/ (float (floor (* minimum range-factor))) range-factor))
         (nice-max (/ (float (ceiling (* maximum range-factor))) range-factor)))
    `(:min ,minimum :max ,maximum :range ,range
      :range-factor ,range-factor
      :nice-min ,nice-min :nice-max ,nice-max :nice-range ,(- nice-max nice-min))))

(defun org--plot/sensible-tick-num (table &optional hard-min hard-max)
  (let* ((row-data
          (mapcar (lambda (row) (org--plot/values-stats
                                 (mapcar #'string-to-number (cdr row))
                                 hard-min
                                 hard-max)) table))
         (row-normalised-ranges (mapcar (lambda (r-data)
                                          (let ((val (round (*
                                                             (plist-get r-data :range-factor)
                                                             (plist-get r-data :nice-range)))))
                                            (if (= (% val 10) 0) (/ val 10) val)))
                                        row-data))
         (range-prime-decomposition (mapcar #'org--plot/prime-factors row-normalised-ranges))
         (weighted-factors (sort (apply #'org--plot/merge-alists #'+ 0
                                        (mapcar (lambda (factors) (org--plot/item-frequencies factors t))
                                                range-prime-decomposition))
                                 (lambda (a b) (> (cdr a) (cdr b))))))
    (apply #'* (org--plot/nice-frequency-pick weighted-factors))))

(defun org--plot/nice-frequency-pick (frequencies)
  (case (length frequencies)
    (1 (list (car (nth 0 frequencies))))
    (2 (if (<= 3 (/ (cdr (nth 0 frequencies))
                    (cdr (nth 1 frequencies))))
           (make-list 2
                      (car (nth 0 frequencies)))
         (list (car (nth 0 frequencies))
               (car (nth 1 frequencies)))))
    (t
     (let* ((total-count (apply #'+ (mapcar #'cdr frequencies)))
            (n-freq (mapcar (lambda (freq) `(,(car freq) . ,(/ (float (cdr freq)) total-count))) frequencies))
            (f-pick (list (car (car n-freq))))
            (1-2-ratio (/ (cdr (nth 0 n-freq))
                          (cdr (nth 1 n-freq))))
            (2-3-ratio (/ (cdr (nth 1 n-freq))
                          (cdr (nth 2 n-freq))))
            (1-3-ratio (* 1-2-ratio 2-3-ratio))
            (1-val (car (nth 0 n-freq)))
            (2-val (car (nth 1 n-freq)))
            (3-val (car (nth 2 n-freq))))
       (when (> 1-2-ratio 4) (push 1-val f-pick))
       (when (and (< 1-2-ratio 2-val)
                  (< (* (apply #'* f-pick) 2-val) 30))
         (push 2-val f-pick))
       (when (and (< 1-3-ratio 3-val)
                  (< (* (apply #'* f-pick) 3-val) 30))
         (push 3-val f-pick))
       f-pick))))

(defun org--plot/merge-alists (function default alist1 alist2 &rest alists)
  (when (> (length alists) 0)
    (setq alist2 (apply #'org--plot/merge-alists function default alist2 alists)))
  (flet ((keys (alist) (mapcar #'car alist))
         (lookup (key alist) (or (cdr (assoc key alist)) default)))
    (loop with keys = (union (keys alist1) (keys alist2) :test 'equal)
          for k in keys collect
          (cons k (funcall function (lookup k alist1) (lookup k alist2))))))

(defun org--plot/item-frequencies (values &optional normalise)
  "Return an alist indicating the frequency of values in VALUES list."
  (let ((normaliser (if normalise (float (length values)) 1)))
    (cl-loop for (n . m) in (seq-group-by #'identity values)
             collect (cons n (/ (length m) normaliser)))))

(defun org--plot/prime-factors (value)
  "Return the prime decomposition of VALUE, e.g. for 12, '(3 2 2)"
  (let ((factors '(1)) (i 1))
    (while (/= 1 value)
      (setq i (1+ i))
      (when (eq 0 (% value i))
        (push i factors)
        (setq value (/ value i))
        (setq i (1- i))
        ))
    (subseq factors 0 -1)))

;;-----------------------------------------------------------------------------
;; facade functions
;;;###autoload
(defun org-plot/gnuplot (&optional params)
  "Plot table using gnuplot.  Gnuplot options can be specified with PARAMS.
If not given options will be taken from the +PLOT
line directly before or after the table."
  (interactive)
  (require 'gnuplot)
  (save-window-excursion
    (delete-other-windows)
    (when (get-buffer "*gnuplot*") ; reset *gnuplot* if it already running
      (with-current-buffer "*gnuplot*"
        (goto-char (point-max))))
    (org-plot/goto-nearest-table)
    ;; Set default options.
    (dolist (pair org-plot/gnuplot-default-options)
      (unless (plist-member params (car pair))
        (setf params (plist-put params (car pair) (cdr pair)))))
    ;; Collect options.
    (save-excursion (while (and (equal 0 (forward-line -1))
                                (looking-at "[[:space:]]*#\\+"))
                      (setf params (org-plot/collect-options params))))
    ;; collect table and table information
    (let* ((data-file (make-temp-file "org-plot"))
           (table (let ((tbl (org-table-to-lisp)))
                    (when (pcase (plist-get params :transpose)
                            ('y   t)
                            ('yes t)
                            ('t   t))
                      (setf tbl (apply #'cl-mapcar #'list
                                       (remove 'hline tbl)))
                      (push 'hline (cdr tbl)))
                    tbl))
           (num-cols (length (if (eq (nth 0 table) 'hline) (nth 1 table)
                               (nth 0 table)))))
      (run-with-idle-timer 0.1 nil #'delete-file data-file)
      (while (eq 'hline (car table)) (setf table (cdr table)))
      (when (eq (cadr table) 'hline)
        (setf params
              (plist-put params :labels (nth 0 table))) ; headers to labels
        (setf table (delq 'hline (cdr table)))) ; clean non-data from table
      (pp params)
      ;; Dump table to datafile (very different for grid).
      (pcase (plist-get params :plot-type)
        (`2d   (org-plot/gnuplot-to-data table data-file params))
        (`3d   (org-plot/gnuplot-to-data table data-file params))
        (`grid (let ((y-labels (org-plot/gnuplot-to-grid-data
                                table data-file params)))
                 (when y-labels (plist-put params :ylabels y-labels)))))
      ;; Check for timestamp ind column.
      (let ((ind (1- (plist-get params :ind))))
        (when (and (>= ind 0) (eq '2d (plist-get params :plot-type)))
          (if (= (length
                  (delq 0 (mapcar
                           (lambda (el)
                             (if (string-match org-ts-regexp3 el) 0 1))
                           (mapcar (lambda (row) (nth ind row)) table))))
                 0)
              (plist-put params :timeind t)
            ;; Check for text ind column.
            (if (or (string= (plist-get params :with) "hist")
                    (> (length
                        (delq 0 (mapcar
                                 (lambda (el)
                                   (if (string-match org-table-number-regexp el)
                                       0 1))
                                 (mapcar (lambda (row) (nth ind row)) table))))
                       0))
                (plist-put params :textind t)))))
      ;; Write script.
      (with-temp-buffer
        (if (plist-get params :script)	; user script
            (progn (insert
                    (org-plot/gnuplot-script table data-file num-cols params t))
                   (insert "\n")
                   (insert-file-contents (plist-get params :script))
                   (goto-char (point-min))
                   (while (re-search-forward "\\$datafile" nil t)
                     (replace-match data-file nil nil)))
          (insert (org-plot/gnuplot-script table data-file num-cols params)))
        ;; Graph table.
        (gnuplot-mode)
        (gnuplot-send-buffer-to-gnuplot))
      ;; Cleanup.
      (bury-buffer (get-buffer "*gnuplot*")))))

(provide 'org-plot)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-plot.el ends here
