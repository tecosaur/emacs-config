#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq default-directory (file-name-directory load-file-name))

(setq message-colour t)
(load (expand-file-name "initialise.el") nil t)

(message "Starting publish process")

;;; Associated processes

(defvar dependent-processes nil)
(defvar dependent-process-names nil)

(defun wait-for-script (file)
  (let ((proc-name (intern (format "%s-process" (file-name-base file)))))
    (set proc-name (start-process (file-name-base file) nil (expand-file-name file)))
    (push (symbol-value proc-name) dependent-processes)
    (push (file-name-base file) dependent-process-names)
    (watch-process (symbol-value proc-name) file)))

(defun watch-process (proc file)
  (set-process-sentinel
   proc
   `(lambda (process _signal)
      (when (eq (process-status process) 'exit)
        (if (= 0 (process-exit-status process))
            (message (format "[1;35] %s finished%s"
                             ,(file-name-base (eval file))
                             (make-string (- (* (1+ max-name-length)
                                                (length dependent-process-names))
                                             ,(length (eval file)))
                                          ? )))
          ;; non-zero exit code
          (message (format "[31] %s process failed!%s"
                           ,(file-name-base (eval file))
                           (make-string (- (* (1+ max-name-length)
                                              (length dependent-process-names))
                                           ,(length (eval file)))
                                        ? )))
          (message "\033[0;31m      %s\033[0m"
                   'unmodified
                   (with-temp-buffer
                     (insert-file-contents-literally (expand-file-name (format "%s-log.txt" (file-name-base file) (file-name-directory load-file-name))))
                     (buffer-substring-no-properties (point-min) (point-max))))
          (setq exit-code 1))))))

;;; Start dependent processes

(wait-for-script "htmlize.sh")

(wait-for-script "org-exporter.sh")

;;; Status info

(defvar max-name-length (apply #'max (cons 8 (mapcar #'length dependent-process-names))))
(defun process-status-table ()
  (message (concat
            "\033[1m[%4.1fs] \033[0;1m"
            (mapconcat (lambda (name)
                         (format (format "%%%ds" max-name-length) name))
                       dependent-process-names " ")
            "\n\033[0m        "
            (mapconcat (lambda (proc)
                         (apply #'format (format "%%s%%%ds" max-name-length)
                                (pcase (process-status proc)
                                  ('run '("\033[0;33m" "Active"))
                                  ('exit '("\033[0;32m" "Complete")))))
                       dependent-processes " ")
            "\033[0;90m[1A[K[1A[K")
           'unmodified
           (- (float-time) start-time)))

;;; Await completion

(setq all-proc-finished nil)

(while (not all-proc-finished)
  (process-status-table)
  (setq all-proc-finished t)
  (dolist (proc dependent-processes)
    (let ((status (process-status proc)))
      (when (not (eq (process-status proc) 'exit))
        (setq all-proc-finished nil))))
  (unless all-proc-finished
    (sleep-for 0.5)))

(delete-file "typescript")

(message "[1;32] Config publish content generated!")

(setq inhibit-message t)
(kill-emacs exit-code)
