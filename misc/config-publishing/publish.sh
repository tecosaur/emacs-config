#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq default-directory (file-name-directory load-file-name))

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el") nil t)

(message "Starting publish process")

(setq timeout 300) ; give up after this many seconds

;;; Associated processes

(defvar dependent-processes nil)
(defvar dependent-process-names nil)

(require 'cl-lib)

(cl-defun wait-for-script (file &key then)
  (let ((proc-name (intern (format "%s-process" (file-name-base file))))
        proc-info)
    (set proc-name (start-process (file-name-base file) nil (expand-file-name file)))
    (setq proc-info (list :proc (symbol-value proc-name)
                          :file file
                          :name (file-name-base file)
                          :padded-name (format "%-8s" (file-name-base file)) ; max len Active/Complete
                          :then (if (listp then) then (list then))))
    (push proc-info dependent-processes)
    (watch-process proc-info)))

(defun watch-process (proc-info)
  (let ((file (plist-get proc-info :file)))
    (set-process-sentinel
     (plist-get proc-info :proc)
     `(lambda (process _signal)
        (when (eq (process-status process) 'exit)
          (if (= 0 (process-exit-status process))
              (progn
                (message (format "[1;35] %s finished%s"
                                 ,(file-name-base file)
                                 (space-fill-line ,(length (file-name-base file)))))
                ;; start dependent processes
                (when ,(car (plist-get proc-info :then))
                  (mapcar (lambda (then) (apply #'wait-for-script (if (listp then) then (list then))))
                          ',(plist-get proc-info :then))))
            ;; non-zero exit code
            (message (format "[31] %s process failed!%s"
                             ,(file-name-base (eval file))
                             (space-fill-line ,(+ 16 (length (file-name-base file))))))
            (message "\033[0;31m      %s\033[0m"
                     'unmodified
                     (with-temp-buffer
                       (insert-file-contents-literally (expand-file-name ,(format "%s-log.txt" (file-name-base file))
                                                                         (file-name-directory load-file-name)))
                       (buffer-substring-no-properties (point-min) (point-max))))
            (message "[1;31] Config publishing aborted%s" (space-fill-line 23))
            (kill-emacs 1)))))))

(defun space-fill-line (base-length)
  "Return whitespace such that the line will be filled to overwrite the status line."
  (make-string (max 0
                    (- (apply #'+ (* 2 (1- (length dependent-processes)))
                              (mapcar (lambda (dep) (length (plist-get dep :padded-name))) dependent-processes))
                       base-length))
               ? ))

;;; Start dependent processes

(wait-for-script "check-package-updates.sh")

(wait-for-script "htmlize.sh")

(if (not (file-exists-p (concat user-emacs-directory "xkcd/")))
    (wait-for-script "org-html.sh" :then "org-pdf.sh")
  (wait-for-script "org-html.sh")
  (wait-for-script "org-pdf.sh"))

;;; Status info

(defun process-status-table ()
  (message (concat
            "\033[1m[%4.1fs] \033[0;1m"
            (mapconcat (lambda (dep) (plist-get dep :padded-name)) dependent-processes "  ")
            "\n\033[0m        "
            (mapconcat (lambda (dep)
                         (apply #'format (format "%%s%%-%ds" (length (plist-get dep :padded-name)))
                                (pcase (process-status (plist-get dep :proc))
                                  ('run '("\033[0;33m" "Active"))
                                  ('exit '("\033[0;32m" "Complete")))))
                       dependent-processes
                       "  ")
            "\033[0;90m[1A[K[1A[K")
           'unmodified
           (- (float-time) start-time)))

;;; Await completion

(setq all-proc-finished nil)

(while (not all-proc-finished)
  (process-status-table)
  (setq all-proc-finished t)
  (dolist (dep dependent-processes)
    (when (not (eq (process-status (plist-get dep :proc)) 'exit))
      (setq all-proc-finished nil)))
  (when (< timeout (- (float-time) start-time))
    (message "[0;31] Timout exceeded. Killing slow processes%s" (space-fill-line 37))
    (dolist (dep dependent-processes)
      (let ((proc (plist-get dep :proc)))
        (when (not (eq (process-status proc) 'exit))
          (message "[1;31] Killing %s%s" proc (space-fill-line (+ 6 (length (format "%s" proc)))))
          (signal-process proc 'SIGUSR2)
          (sleep-for 0.2)
          (delete-process proc)
          (message "\n\033[0;31m      %s\033[0m"
                   'unmodified
                   (with-temp-buffer
                     (insert-file-contents-literally (expand-file-name (format "%s-log.txt" (file-name-base (plist-get dep :file)))
                                                                       (file-name-directory load-file-name)))
                     (buffer-substring-no-properties (point-min) (point-max)))))))
    (setq all-proc-finished t)
    (setq exit-code 1))
  (unless all-proc-finished
    (sleep-for 0.5)))

(if (= 0 exit-code)
    (message "[1;32] Config publish content generated!%s" (space-fill-line 33))
  (message "[1;31] Config publishing aborted%s" (space-fill-line 25)))

(setq inhibit-message t)
(kill-emacs exit-code)
