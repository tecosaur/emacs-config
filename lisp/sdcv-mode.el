;;; sdcv-mode.el --- major mode to do dictionary query through sdcv

;; Copyright 2006~2008 pluskid,
;;           2011 gucong
;;
;; Author: pluskid <pluskid@gmail.com>,
;;         gucong <gucong43216@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a major mode to view output of dictionary search of sdcv.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'sdcv-mode)
;;   (global-set-key (kbd "C-c d") 'sdcv-search)

;;; Changelog:

;; 2012/01/02
;;     * New variable: `sdcv-word-processor'
;;     * Breaking change:
;;       for `sdcv-dictionary-list' and `sdcv-dictionary-alist',
;;       non-list (non-nil) value now means full dictionary list
;;     * Rewrite `sdcv-search' for both interactive and non-interactive use
;;     * `sdcv-dictionary-list' is left for customization use only
;;     * Better highlighting.
;;
;; 2011/06/30
;;     * New feature: parse output for failed lookup
;;     * Keymap modification
;;
;; 2008/06/11
;;     * sdcv-mode v 0.1 init (with background process)

;;; Code:

(require 'outline)
(provide 'sdcv-mode)
(eval-when-compile
  (require 'cl))

;;; ==================================================================
;;; Frontend, search word and display sdcv buffer
(defun sdcv-search (word &optional dict-list-name dict-list interactive-p)
  "Search WORD through the command-line tool sdcv.
The result will be displayed in buffer named with
`sdcv-buffer-name' with `sdcv-mode' if called interactively.

When provided with DICT-LIST-NAME, query `sdcv-dictionary-alist'
to get the new dictionary list before search.
Alternatively, dictionary list can be specified directly
by DICT-LIST.  Any non-list value of it means using all dictionaries.

When called interactively, prompt for the word.
Prefix argument have the following meaning:
If `sdcv-dictionary-alist' is defined,
use prefix argument to select a new DICT-LIST-NAME.
Otherwise, prefix argument means using all dictionaries.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
    \       escape the character right after"
  (interactive
   (let* ((dict-list-name
           (and current-prefix-arg sdcv-dictionary-alist
                (completing-read "Select dictionary list: "
                                 sdcv-dictionary-alist nil t)))
          (dict-list
           (and current-prefix-arg (not sdcv-dictionary-alist)))
          (guess (or (and transient-mark-mode mark-active
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))
                     (current-word nil t)))
          (word (read-string (format "Search dict (default: %s): " guess)
                             nil nil guess)))
     (list word dict-list-name dict-list t)))
  ;; init current dictionary list
  (when (null sdcv-current-dictionary-list)
    (setq sdcv-current-dictionary-list sdcv-dictionary-list))
  ;; dict-list-name to dict-list
  (when (and (not dict-list) dict-list-name)
    (if (not sdcv-dictionary-alist)
        (error "`sdcv-dictionary-alist' not defined"))
    (setq dict-list
          (cdr (assoc dict-list-name sdcv-dictionary-alist))))
  ;; prepare new dictionary list
  (when (and dict-list (not (equal sdcv-current-dictionary-list dict-list)))
    (setq sdcv-current-dictionary-list dict-list)
    ;; kill sdcv process
    (and (get-process sdcv-process-name)
         (kill-process (get-process sdcv-process-name)))
    (while (get-process sdcv-process-name)
      (sleep-for 0.01)))
  (let ((result
         (concat ">>>"
          (mapconcat
           (lambda (w) (sdcv-do-lookup w))
           (if sdcv-word-processor
               (let ((processed (funcall sdcv-word-processor word)))
                 (if (listp processed) processed (list processed)))
             (list word))
           ">>>"))))
    (if (not interactive-p)
        result
      (with-current-buffer (get-buffer-create sdcv-buffer-name)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert result))
      (sdcv-goto-sdcv)
      (sdcv-mode)
      (sdcv-mode-reinit))))

(defun sdcv-list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command "sdcv -l" sdcv-buffer-name)))

(defvar sdcv-current-dictionary-list nil)

(defun sdcv-generate-dictionary-argument ()
  "Generate dictionary argument for sdcv from `sdcv-current-dictionary-list'
and `sdcv-dictionary-path'."
  (append
   (and sdcv-dictionary-path (list "--data-dir" sdcv-dictionary-path))
   (and (listp sdcv-current-dictionary-list)
        (mapcan (lambda (dict)
                  (list "-u" dict))
                sdcv-current-dictionary-list))))

;;; ==================================================================
;;; utilities to switch from and to sdcv buffer
(defvar sdcv-previous-window-conf nil
  "Window configuration before switching to sdcv buffer.")
(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (interactive)
  (unless (eq (current-buffer)
	      (sdcv-get-buffer))
    (setq sdcv-previous-window-conf (current-window-configuration)))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))
(defun sdcv-return-from-sdcv ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-conf)
      (progn
        (set-window-configuration sdcv-previous-window-conf)
        (setq sdcv-previous-window-conf nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer. Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

;;; ==================================================================
;;; The very major mode
(defvar sdcv-mode-font-lock-keywords
  '(
    ;; dictionary name
    ("^-->\\(.*\\)$" . (1 sdcv-hit-face))
    ("^==>\\(.*\\)$" . (1 sdcv-failed-face))
    ("^\\(>>>.*\\)$" . (1 sdcv-heading-face))
    )
  "Expressions to hilight in `sdcv-mode'")

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'sdcv-return-from-sdcv)
    (define-key map (kbd "RET") 'sdcv-search)
    (define-key map "a" 'show-all)
    (define-key map "h" 'hide-body)
    (define-key map "o" 'sdcv-toggle-entry)
    (define-key map "n" 'sdcv-next-entry)
    (define-key map "p" 'sdcv-previous-entry)
    map)
  "Keymap for `sdcv-mode'.")

(define-derived-mode sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq font-lock-defaults '(sdcv-mode-font-lock-keywords))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "-->.*\n-->\\|==>\\|>>>")
  (set (make-local-variable font-lock-string-face) nil)
)

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entrys but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only nil)
    (sdcv-parse-failed)
    (setq buffer-read-only t)
    (hide-body)
    (goto-char (point-min))
    (forward-line 1)
    (show-entry)))

(defun sdcv-parse-failed ()
  (goto-char (point-min))
  (let (save-word)
    (while (re-search-forward "^[0-9]+).*-->\\(.*\\)$" nil t)
      (let ((cur-word (match-string-no-properties 1)))
        (unless (string= save-word cur-word)
          (setq save-word cur-word)
          (re-search-backward "^\\(.\\)" nil t)
          (match-string 1)
          (insert (format "\n==>%s\n" save-word)))))))

(defun sdcv-next-entry ()
  (interactive)
  (outline-next-heading)
  (show-entry)
  (recenter-top-bottom 0))
(defun sdcv-previous-entry ()
  (interactive)
  (outline-previous-heading)
  (show-entry)
  (recenter-top-bottom 0))

(defun sdcv-toggle-entry ()
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (hide-entry)
      (show-entry))))

;;; ==================================================================
;;; Support for sdcv process in background
(defun sdcv-do-lookup (word)
  "Send the word to the sdcv process and return the result."
  (let ((process (sdcv-get-process)))
    (process-send-string process (concat word "\n"))
    (with-current-buffer (process-buffer process)
      (let ((i 0) rlt done)
	(while (and (not done)
		    (< i sdcv-wait-timeout))
	  (when (sdcv-match-tail sdcv-word-prompts)
	    (setq rlt (buffer-substring-no-properties (point-min)
						      (point-max)))
	    (setq done t))
	  (when (sdcv-match-tail sdcv-choice-prompts)
	    (process-send-string process "-1\n"))
	  (unless done
	    (sleep-for sdcv-wait-interval)
	    (setq i (+ i sdcv-wait-interval))))
	(unless (< i sdcv-wait-timeout)
	  ;; timeout
	  (kill-process process)
	  (error "ERROR: timeout waiting for sdcv"))
	(erase-buffer)
    rlt))))

(defvar sdcv-wait-timeout 2
  "The max time (in seconds) to wait for the sdcv process to
produce some output.")
(defvar sdcv-wait-interval 0.01
  "The interval (in seconds) to sleep each time to wait for
sdcv's output.")

(defconst sdcv-process-name "%sdcv-mode-process%")
(defconst sdcv-process-buffer-name "*sdcv-mode-process*")

(defvar sdcv-word-prompts '("Enter word or phrase: "
			    "请输入单词或短语："
			    "請輸入單字或片語：")
  "A list of prompts that sdcv use to prompt for word.")

(defvar sdcv-choice-prompts '("Your choice[-1 to abort]: "
			      "您的选择为："
			      "您的選擇為：")
  "A list of prompts that sdcv use to prompt for a choice
of multiple candicates.")

(defvar sdcv-result-patterns '("^Found [0-9]+ items, similar to [*?/|]*\\(.+?\\)[*?]*\\."
			      "^发现 [0-9]+ 条记录和 [*?/|]*\\(.+?\\)[*?]* 相似。"
			      )
  "A list of patterns to extract result word of sdcv. Special
characters are stripped.")

(defun sdcv-get-process ()
  "Get or create the sdcv process."
  (let ((process (get-process sdcv-process-name)))
    (when (null process)
      (with-current-buffer (get-buffer-create
			    sdcv-process-buffer-name)
	(erase-buffer)
	(setq process (apply 'start-process
			     sdcv-process-name
			     sdcv-process-buffer-name
			     sdcv-program-path
			     (sdcv-generate-dictionary-argument)))
	;; kill the initial prompt
	(let ((i 0))
	  (message "starting sdcv...")
	  (while (and (not (sdcv-match-tail sdcv-word-prompts))
		      (< i sdcv-wait-timeout))
	    (sleep-for sdcv-wait-interval)
	    (setq i (+ i sdcv-wait-interval)))
	  (unless (< i sdcv-wait-timeout)
	    ;; timeout
	    (kill-process process)
	    (error "ERROR: timeout waiting for sdcv"))
	  (erase-buffer))))
    process))

(defun sdcv-buffer-tail (length)
  "Get a substring of length LENGTH at the end of
current buffer."
  (let ((beg (- (point-max) length))
	(end (point-max)))
    (if (< beg (point-min))
	(setq beg (point-min)))
    (buffer-substring-no-properties beg end)))

(defun sdcv-match-tail (prompts)
  (let ((done nil)
	(prompt nil))
    (while (and (not done)
		prompts)
      (setq prompt (car prompts))
      (setq prompts (cdr prompts))
      (when (string-equal prompt
                          (sdcv-buffer-tail (length prompt)))
        (delete-region (- (point-max) (length prompt))
                       (point-max))
        (setq done t)))
    done))


;;;;##################################################################
;;;;  User Options, Variables
;;;;##################################################################

(defvar sdcv-buffer-name "*sdcv*"
  "The name of the buffer of sdcv.")
(defvar sdcv-dictionary-list t
  "A list of dictionaries to use.
Each entry is a string denoting the name of a dictionary, which
is then passed to sdcv through the '-u' command line option.
Any non-list value means using all the dictionaries.")
(defvar sdcv-dictionary-alist nil
  "An alist of dictionaries, used to interactively form
dictionary list. It has the form:
   ((\"full\" . t)
    (\"group1\" \"dict1\" \"dict2\" ...)
    (\"group2\" \"dict2\" \"dict3\"))
Any cons cell here means using all dictionaries.
")

(defvar sdcv-program-path "sdcv"
  "The path of sdcv program.")

(defvar sdcv-dictionary-path nil
  "The path of dictionaries.")

(defvar sdcv-word-processor nil
  "This is the function that take a word (stirng)
and return a word or a list of words for lookup by `sdcv-search'.
All lookup result(s) will finally be concatenated together.

`nil' value means do nothing with the original word.

The following is an example.  This function takes the original word and
compare whether simplified and traditional form of the word are the same.
If not, look up both of the words.

      (lambda (word)
        (let ((sim (chinese-conv word \"simplified\"))
              (tra (chinese-conv word \"traditional\")))
          (if (not (string= sim tra))
              (list sim tra)
            word)))
")

(defvar sdcv-hit-face 'font-lock-type-face
  "Face for search hits")
(defvar sdcv-failed-face 'font-lock-keyword-face
  "Face for suggestions for a failed lookup.")
(defvar sdcv-heading-face 'highlight
  "Face for heading of lookup")

;;; sdcv-mode.el ends here
