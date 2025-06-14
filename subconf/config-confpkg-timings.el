;;; config-confpkg-timings.el --- Generated package (no.1) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.1) from my config.
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

(defvar confpkg-load-time-tree (list (list 'root)))
(defvar confpkg-record-branch (list 'root))
(defvar confpkg-record-num 0)

(defun confpkg-create-record (name elapsed &optional parent enclosing)
  (let ((parent (assoc (or parent (car confpkg-record-branch))
                       confpkg-load-time-tree))
        (record (cons name (list (list 'self
                                       :name (format "%s" name)
                                       :num (cl-incf confpkg-record-num)
                                       :elapsed elapsed
                                       :enclosing enclosing)))))
    (push record confpkg-load-time-tree)
    (push record (cdr parent))
    record))

(defun confpkg-start-record (name &optional parent)
  (let ((record (confpkg-create-record name 0.0e+NaN parent t)))
    (plist-put (cdadr record) :start (float-time))
    (push name confpkg-record-branch)
    record))

(defun confpkg-finish-record (name)
  (let ((self-record (cdar (last (cdr (assoc name confpkg-load-time-tree))))))
    (plist-put self-record :elapsed
               (- (float-time) (plist-get self-record :start) 0.0))
    (unless (equal (car confpkg-record-branch) name)
      (message "Warning: Confpkg timing record expected to finish %S, instead found %S. %S"
               name (car confpkg-record-branch) confpkg-record-branch))
    (setq confpkg-record-branch (cdr confpkg-record-branch))))

(defmacro confpkg-with-record (name &rest body)
  "Create a time record around BODY.
The record must have a NAME."
  (declare (indent 1))
  (let ((name-val (make-symbol "name-val"))
        (record-spec (make-symbol "record-spec")))
    `(let* ((,name-val ,name)
            (,record-spec (if (consp ,name-val) ,name-val (list ,name-val))))
       (apply #'confpkg-start-record ,record-spec)
       (unwind-protect
           (progn ,@body)
         (confpkg-finish-record (car ,record-spec))))))

(defadvice! +require--log-timing-a (orig-fn feature &optional filename noerror)
  :around #'require
  (if (or (featurep feature)
          (eq feature 'cus-start) ; HACK Why!?!
          (assoc (format "require: %s" feature) confpkg-load-time-tree))
      (funcall orig-fn feature filename noerror)
    (confpkg-with-record (list (format "require: %s" feature)
                               (and (eq (car confpkg-record-branch) 'root)
                                    'requires))
      (funcall orig-fn feature filename noerror))))

(defun confpkg-timings-report (&optional sort-p node)
  "Display a report on load-time information.
Supply SORT-P (or the universal argument) to sort the results.
NODE defaults to the root node."
  (interactive
   (list (and current-prefix-arg t)))
  (let ((buf (get-buffer-create "*Confpkg Load Time Report*"))
        (depth 0)
        num-pad name-pad max-time max-total-time max-depth)
    (cl-labels
        ((sort-records-by-time
          (record)
          (let ((self (assoc 'self record)))
            (append (list self)
                    (sort (nreverse (remove self (cdr record)))
                          (lambda (a b)
                            (> (or (plist-get (alist-get 'self a) :total) 0.0)
                               (or (plist-get (alist-get 'self b) :total) 0.0)))))))
         (print-record
          (record)
          (cond
           ((eq (car record) 'self)
            (insert
             (propertize
              (string-pad (number-to-string (plist-get (cdr record) :num)) num-pad)
              'face 'font-lock-keyword-face)
             " "
             (propertize
              (apply #'concat
                     (make-list (1- depth) "• "))
              'face 'font-lock-comment-face)
             (string-pad (format "%s" (plist-get (cdr record) :name)) name-pad)
             (make-string (* (- max-depth depth) 2) ?\s)
             (propertize
              (format "%.4fs" (plist-get (cdr record) :elapsed))
              'face
              (list :foreground
                    (doom-blend 'orange 'green
                                (/ (plist-get (cdr record) :elapsed) max-time))))
             (if (= (plist-get (cdr record) :elapsed)
                    (plist-get (cdr record) :total))
                 ""
               (concat "   (Σ="
                       (propertize
                        (format "%.3fs" (plist-get (cdr record) :total))
                        'face
                        (list :foreground
                              (doom-blend 'orange 'green
                                          (/ (plist-get (cdr record) :total) max-total-time))))
                       ")"))
             "\n"))
           (t
            (cl-incf depth)
            (mapc
             #'print-record
             (if sort-p
                 (sort-records-by-time record)
               (reverse (cdr record))))
            (cl-decf depth))))
         (flatten-records
          (records)
          (if (eq (car records) 'self)
              (list records)
            (mapcan
             #'flatten-records
             (reverse (cdr records)))))
         (tree-depth
          (records &optional depth)
          (if (eq (car records) 'self)
              (or depth 0)
            (1+ (cl-reduce #'max (cdr records) :key #'tree-depth))))
         (mapreduceprop
          (list map reduce prop)
          (cl-reduce
           reduce list
           :key
           (lambda (p) (funcall map (plist-get (cdr p) prop)))))
         (elaborate-timings
          (record)
          (if (eq (car record) 'self)
              (plist-get (cdr record) :elapsed)
            (let ((total (cl-reduce #'+ (cdr record)
                                    :key #'elaborate-timings))
                  (self (cdr (assoc 'self record))))
              (if (plist-get self :enclosing)
                  (prog1
                      (plist-get self :elapsed)
                    (plist-put self :total (plist-get self :elapsed))
                    (plist-put self :elapsed
                               (- (* 2 (plist-get self :elapsed)) total)))
                (plist-put self :total total)
                total))))
         (elaborated-timings
          (record)
          (let ((record (copy-tree record)))
            (elaborate-timings record)
            record)))
      (let* ((tree
              (elaborated-timings
               (append '(root)
                       (copy-tree
                        (alist-get (or node 'root)
                                   confpkg-load-time-tree
                                   nil nil #'equal))
                       '((self :num 0 :elapsed 0)))))
             (flat-records
              (cl-remove-if
               (lambda (rec) (= (plist-get (cdr rec) :num) 0))
               (flatten-records tree))))
        (setq max-time (mapreduceprop flat-records #'identity #'max :elapsed)
              max-total-time (mapreduceprop flat-records #'identity #'max :total)
              name-pad (mapreduceprop flat-records #'length #'max :name)
              num-pad (mapreduceprop flat-records
                                     (lambda (n) (length (number-to-string n)))
                                     #'max :num)
              max-depth (tree-depth tree))
        (with-current-buffer buf
          (erase-buffer)
          (setq-local outline-regexp "[0-9]+ *\\(?:• \\)*")
          (outline-minor-mode 1)
          (use-local-map (make-sparse-keymap))
          (local-set-key "TAB" #'outline-toggle-children)
          (local-set-key "\t" #'outline-toggle-children)
          (local-set-key (kbd "<backtab>") #'outline-show-subtree)
          (local-set-key (kbd "C-<iso-lefttab>")
                         (eval `(cmd! (if current-prefix-arg
                                          (outline-show-all)
                                        (outline-hide-sublevels (+ ,num-pad 2))))))
          (insert
           (propertize
            (concat (string-pad "#" num-pad) " "
                    (string-pad "Confpkg"
                                (+ name-pad (* 2 max-depth) -3))
                    (format " Load Time (Σ=%.3fs)\n"
                            (plist-get (cdr (assoc 'self tree)) :total)))
            'face '(:inherit (tab-bar-tab bold) :extend t :underline t)))
          (dolist (record (if sort-p
                              (sort-records-by-time tree)
                            (reverse (cdr tree))))
            (unless (eq (car record) 'self)
              (print-record record)))
          (set-buffer-modified-p nil)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(provide 'config-confpkg-timings)
;;; config-confpkg-timings.el ends here
