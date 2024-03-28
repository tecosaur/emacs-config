;;; autocorrect.el --- Generated package (no.26) from my config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <https://code.tecosaur.net/tec>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 28, 2024
;; Modified: March 28, 2024
;; Version: 2024.03.28
;; Homepage: https://code.tecosaur.net/tec/emacs-config
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Generated package (no.26) from my config.
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

(require 'jinx)

(defgroup autocorrect nil
  "Automatically fix typos and frequent spelling mistakes."
  :group 'text
  :prefix "autocorrect-")

(defcustom autocorrect-history-file
  (file-name-concat (or (getenv "XDG_STATE_HOME") "~/.local/state")
                    "emacs" "spelling-corrections.txt")
  "File where a spell check record will be saved."
  :type 'file)

(defvar autocorrect-record-table (make-hash-table :test #'equal)
  "A record of all corrections made.
Misspelled words are the keys, and a alist of corrections and their count are
the values.")

(defcustom autocorrect-count-threshold-history 3
  "The number of recorded identical misspellings to create an abbrev.
This applies to misspellings read from the history file"
  :type 'natnum)

(defcustom autocorrect-count-threshold-session 2
  "The number of identical misspellings to create an abbrev.
This applies to misspellings made in the current Emacs session."
  :type 'natnum)

(defun autocorrect-update-table (misspelling corrected)
  "Update the MISSPELLING to CORRECTED entry in the table.
Returns the number of times this correction has occurred."
  (if-let ((correction-counts
            (gethash misspelling autocorrect-record-table)))
      (if-let ((record-cons (assoc corrected correction-counts)))
          (setcdr record-cons (1+ (cdr record-cons)))
        (puthash misspelling
                 (push (cons corrected 1) correction-counts)
                 autocorrect-record-table)
        1)
    (puthash misspelling
             (list (cons corrected 1))
             autocorrect-record-table)
    1))

;;;###autoload
(define-minor-mode autocorrect-mode
  "Automatically correct misspellings with abbrev."
  :init-value t)

;;;###autoload
(define-globalized-minor-mode global-autocorrect-mode
  autocorrect-mode autocorrect--enable)

(defun autocorrect--enable ()
  "Turn on `autocorrect-mode' in the current buffer."
  (autocorrect-mode 1))


(defcustom autocorrect-predicates nil
  "Predicate functions called at point with argument START.
These functions should return t if autocorrection is valid at START."
  :type '(repeat function))

(defun autocorrect--appropriate-p ()
  "Return non-nil it is currently appropriate to make an autocorrection.
See `autocorrect-predicates'."
  (and autocorrect-mode
       (run-hook-with-args-until-failure 'autocorrect-predicates (point))))

(defcustom autocorrect-abbrev-file
  (file-name-concat (or (getenv "XDG_STATE_HOME") "~/.local/state")
                    "emacs" "spelling-abbrevs.el")
  "File to save spell check records in."
  :type 'file)

(defvar autocorrect-abbrev-table nil
  "The spelling abbrev table.")

(defvar autocorrect-abbrev-table--saved-version 0
  "The version of `autocorrect-abbrev-table' saved to disk.")

(defun autocorrect--setup-abbrevs ()
  "Setup `autocorrect-abbrev-table'.
Also set it as a parent of `global-abbrev-table'."
  (unless autocorrect-abbrev-table
    (setq autocorrect-abbrev-table
          (make-abbrev-table (list :enable-function #'autocorrect--appropriate-p)))
    (abbrev-table-put
     global-abbrev-table :parents
     (cons autocorrect-abbrev-table
           (abbrev-table-get global-abbrev-table :parents)))
    (add-hook 'kill-emacs-hook #'autocorrect-save-abbrevs))
  (when (file-exists-p autocorrect-abbrev-file)
    (read-abbrev-file autocorrect-abbrev-file t)
    (setq autocorrect-abbrev-table--saved-version
          (abbrev-table-get autocorrect-abbrev-table
                            :abbrev-table-modiff))))

(defun autocorrect-save-abbrevs ()
  "Write `autocorrect-abbrev-table'."
  (when (> (abbrev-table-get autocorrect-abbrev-table
                             :abbrev-table-modiff)
           autocorrect-abbrev-table--saved-version)
    (unless (file-exists-p autocorrect-abbrev-file)
      (make-directory (file-name-directory autocorrect-abbrev-file) t))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-buffer
        (insert-abbrev-table-description 'autocorrect-abbrev-table nil)
        (when (unencodable-char-position (point-min) (point-max) 'utf-8)
          (setq coding-system-for-write 'utf-8-emacs))
        (goto-char (point-min))
        (insert (format ";;-*-coding: %s;-*-\n\n" coding-system-for-write))
        (write-region nil nil autocorrect-abbrev-file)))
    (setq autocorrect-abbrev-table--saved-version
          (abbrev-table-get autocorrect-abbrev-table
                            :abbrev-table-modiff))))

(defcustom autocorrect-check-spelling-function nil
  "Predicate function that indicates whether a word is correctly spelt.
This is used to check whether a correction can be safely lowercased."
  :type '(choice function (const nil)))

(defun autocorrect--should-downcase-p (word)
  "Check whether it is a good idea to downcase WORD.
This is conditional on all of the following being true:
- WORD starts with a capital letter
- The rest of WORD is either entirely lower or upper case
  (i.e. WORD is like \"Capitalised\" or \"UPPERCASE\")
- The lowercase form of WORD satisfies `autocorrect-check-spelling-function'"
  (and autocorrect-check-spelling-function
       (char-uppercase-p (aref word 0))
       (let ((letter-cases (mapcar #'char-uppercase-p word)))
         (or (not (memq t (cdr letter-cases)))
             (not (memq nil (cdr letter-cases)))))
       (funcall autocorrect-check-spelling-function
                (downcase word))))

(defun autocorrect-record-correction (misspelling corrected)
  "Record the correction of MISSPELLING to CORRECTED."
  (when (autocorrect--should-downcase-p corrected)
    (setq misspelling (downcase misspelling)
          corrected (downcase corrected)))
  (let ((write-region-inhibit-fsync t) ; Quicker writes
        (coding-system-for-write 'utf-8)
        (inhibit-message t))
    (write-region
     (concat misspelling " " corrected "\n") nil
     autocorrect-history-file t))
  (when (and (>= (autocorrect-update-table misspelling corrected)
                 autocorrect-count-threshold-session)
             (= (length (gethash misspelling autocorrect-record-table))
                1))
    (define-abbrev autocorrect-abbrev-table misspelling corrected)
    (message "Created new autocorrection: %s ⟶ %s"
             (propertize misspelling 'face 'warning)
             (propertize corrected 'face 'success))))

(defun autocorrect--read-history ()
  "Read the history file into the correction table."
  (if (file-exists-p autocorrect-history-file)
      (with-temp-buffer
        (insert-file-contents autocorrect-history-file)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((pt (point))
                misspelling corrected)
            (setq misspelling
                  (and (forward-word)
                       (buffer-substring pt (point)))
                  pt (1+ (point)))
            (setq corrected
                  (and (forward-word)
                       (buffer-substring pt (point)))
                  pt (point))
            (when (and misspelling corrected)
              (autocorrect-update-table misspelling corrected))
            (forward-line 1))))
    (make-directory (file-name-directory autocorrect-history-file))
    (write-region "" nil autocorrect-history-file)))

(defun autocorrect--remove-invalid-abbrevs ()
  "Ensure that all entries of the abbrev table are valid."
  (obarray-map
   (lambda (misspelling-symb)
     (let ((misspelling (symbol-name misspelling-symb)))
       (unless (string-empty-p misspelling) ; Abbrev uses an empty symbol for metadata.
         (let ((corrections (gethash misspelling autocorrect-record-table)))
           (unless (and (= (length corrections) 1)
                        (>= (cdar corrections)
                            autocorrect-count-threshold-history))
             (define-abbrev autocorrect-abbrev-table misspelling nil)
             (unintern misspelling-symb autocorrect-abbrev-table))))))
   autocorrect-abbrev-table))

(defun autocorrect--create-history-abbrevs ()
  "Apply the history threshold to the current correction table."
  (maphash
   (lambda (misspelling corrections)
     (when (and (= (length corrections) 1)
                (>= (cdar corrections)
                    autocorrect-count-threshold-history))
       (unless (obarray-get autocorrect-abbrev-table misspelling)
         (define-abbrev autocorrect-abbrev-table
           misspelling (caar corrections)))))
   autocorrect-record-table))

(defun autocorrect-setup ()
  "Read and process the history file into abbrevs."
  (autocorrect--read-history)
  (autocorrect--setup-abbrevs)
  (autocorrect--remove-invalid-abbrevs)
  (autocorrect--create-history-abbrevs))

(run-with-idle-timer 0.5 nil #'autocorrect-setup)

(defun autocorrect-jinx-record-correction (overlay corrected)
  "Record that Jinx corrected the text in OVERLAY to CORRECTED."
  (let ((text
         (buffer-substring-no-properties
          (overlay-start overlay)
          (overlay-end overlay))))
    (autocorrect-record-correction text corrected)))

(defun autocorrect-jinx-check-spelling (word)
  "Check if WORD is valid."
  ;; Mostly a copy of `jinx--word-valid-p', just without the buffer substring.
  ;; It would have been nice if `jinx--word-valid-p' implemented like this
  ;; with `jinx--this-word-valid-p' (or similar) as the at-point variant.
  (or (member word jinx--session-words)
      ;; Allow capitalized words
      (and (string-match-p "\\`[[:upper:]][[:lower:]]+\\'" word)
           (cl-loop
            for w in jinx--session-words
            thereis (and (string-equal-ignore-case word w)
                         (string-match-p "\\`[[:lower:]]+\\'" w))))
      (cl-loop for dict in jinx--dicts
               thereis (jinx--mod-check dict word))))

(defun autocorrect-jinx-appropriate (pos)
  "Return non-nil if it is appropriate to spellcheck at POS according to jinx."
  (and (not (jinx--face-ignored-p pos))
       (not (jinx--regexp-ignored-p pos))))

(setq autocorrect-check-spelling-function #'autocorrect-jinx-check-spelling)
(add-to-list 'autocorrect-predicates #'autocorrect-jinx-appropriate)
(advice-add 'jinx--correct-replace :before #'autocorrect-jinx-record-correction)

(provide 'autocorrect)
;;; autocorrect.el ends here
