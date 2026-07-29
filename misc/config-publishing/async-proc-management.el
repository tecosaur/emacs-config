;;; misc/config-publishing/async-proc-management.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar start-time (float-time))
(defvar exit-code 0)

(defvar apm-timeout 300
  "Give up after this many seconds")
(defvar apm-dependent-processes nil)

(cl-defmacro apm-exec (files &key then)
  (if then
      (if (listp then)
          `(apm--exec ,files :then ',then)
        `(apm--exec ,files :then ,then))
    `(apm--exec ,files)))

(cl-defun apm--exec (files &key then announce)
  (dolist (file (if (listp files) files (list files)))
    (let ((proc-name (intern (format "%s-process" (file-name-base file))))
          proc-info)
      (when announce
        (message "[0;32] Starting %s%s" file
                 (apm-space-fill-line (+ 9 (length file)))))
      (set proc-name
           (start-process (file-name-base file)
                          (generate-new-buffer
                           (format " *apm-%s*" (file-name-base file)))
                          (if (file-exists-p file)
                              (expand-file-name file)
                            file)))
      (setq proc-info (list :proc (symbol-value proc-name)
                            :file file
                            :name (file-name-base file)
                            :padded-name (format "%-8s" (file-name-base file)) ; max len Active/Complete
                            :then (if (listp then) then (list then))))
      (push proc-info apm-dependent-processes)
      (apm-watch-process proc-info))))

(defun apm-failure-report (name proc)
  "Return a diagnostic report for failed stage NAME, or nil if nothing was captured.
Combines the stage's log file (`message' output only) with PROC's buffer, which
is where a dying --script prints its backtrace."
  (let* ((log-file (expand-file-name (format "%s-log.txt" name) script-root))
         (logged (and (file-exists-p log-file)
                      (with-temp-buffer
                        (insert-file-contents-literally log-file)
                        (string-trim (buffer-string)))))
         (output (and (buffer-live-p (process-buffer proc))
                      (with-current-buffer (process-buffer proc)
                        (string-trim (buffer-string)))))
         ;; The log is a prefix of stdout whenever both were captured; showing
         ;; only the longer avoids printing every stage twice.
         (report (if (and logged output (string-prefix-p logged output))
                     output
                   (string-join (delq nil (list logged output)) "\n"))))
    (and (not (string-empty-p (or report ""))) report)))

(defun apm-report-failure (name proc)
  "Print why stage NAME failed, tailing its captured output."
  (message "[31] %s process failed (exit %s)%s"
           name (process-exit-status proc)
           (apm-space-fill-line (+ 26 (length name))))
  (let ((report (apm-failure-report name proc)))
    (if (not report)
        (message "\033[0;31m      (no output captured from %s)\033[0m" name)
      (dolist (line (last (split-string report "\n") 40))
        (message "\033[0;31m      %s\033[0m" line)))))

(defun apm-watch-process (proc-info)
  (let ((file (plist-get proc-info :file)))
    (set-process-sentinel
     (plist-get proc-info :proc)
     `(lambda (process _signal)
        (when (eq (process-status process) 'exit)
          (if (= 0 (process-exit-status process))
              (progn
                (message (format "[1;35] %s finished%s"
                                 ,(file-name-base file)
                                 (apm-space-fill-line ,(length (file-name-base file)))))
                ;; start dependent processes
                (when ',(car (plist-get proc-info :then))
                  (mapcar (lambda (then)
                            (apply #'apm--exec
                                   (append
                                    (pcase (if (and (listp then)
                                                    (symbolp (car then)))
                                               (eval then) then)
                                      ((and n (pred stringp)) (list n))
                                      ((and n (pred listp)) n)
                                      (n (user-error "unrecognised :then form %s" n)))
                                    '(:announce t))))
                          ',(list (plist-get proc-info :then)))))
            ;; non-zero exit code
            (apm-report-failure ,(file-name-base file) process)
            (setq exit-code 1)))))))

(defun apm-space-fill-line (base-length)
  "Return whitespace such that the line will be filled to overwrite the status line."
  (make-string (max 0
                    (- (apply #'+ (* 2 (1- (length apm-dependent-processes)))
                              (mapcar (lambda (dep) (length (plist-get dep :padded-name))) apm-dependent-processes))
                       base-length))
               ? ))


;;; Status info

(defun apm-process-status-table ()
  ;; (message (concat
  ;;           "\033[1m[%4.1fs] \033[0;1m"
  ;;           (mapconcat (lambda (dep) (plist-get dep :padded-name)) apm-dependent-processes "  ")
  ;;           "\n\033[0m        "
  ;;           (mapconcat (lambda (dep)
  ;;                        (apply #'format (format "%%s%%-%ds" (length (plist-get dep :padded-name)))
  ;;                               (pcase (process-status (plist-get dep :proc))
  ;;                                 ('run '("\033[0;33m" "Active"))
  ;;                                 ('exit '("\033[0;32m" "Complete"))
  ;;                                 (_ '("\033[0;31m" (format "Unrecognised status %s"
  ;;                                                           (process-status (plist-get dep :proc))))))))
  ;;                      apm-dependent-processes
  ;;                      "  ")
  ;;           "\033[0;90m[1A[K[1A[K")
  ;;          (- (float-time) start-time))
  )

;;; Await completion

(defun apm-wait-and-monitor ()
  (setq apm-all-proc-finished nil)

  (sleep-for 0.1) ; let processes start
  (while (not apm-all-proc-finished)
    (apm-process-status-table)
    (setq apm-all-proc-finished t)
    (dolist (dep apm-dependent-processes)
      (when (not (eq (process-status (plist-get dep :proc)) 'exit))
        (setq apm-all-proc-finished nil)))
    (when (< apm-timeout (- (float-time) start-time))
      (message "[0;31] Timeout exceeded. Killing slow processes%s" (apm-space-fill-line 37))
      (dolist (dep apm-dependent-processes)
        (let ((proc (plist-get dep :proc)))
          (when (not (eq (process-status proc) 'exit))
            (message "[1;31] Killing %s%s" proc (apm-space-fill-line (+ 6 (length (format "%s" proc)))))
            (signal-process proc 'SIGUSR2)
            (sleep-for 0.2)
            (delete-process proc)
            (let ((report (apm-failure-report (plist-get dep :name) proc)))
              (dolist (line (last (split-string (or report "(no output captured)") "\n") 40))
                (message "\033[0;31m      %s\033[0m" line))))))
      (setq apm-all-proc-finished t)
      (setq exit-code 1))
    (unless apm-all-proc-finished
      (sleep-for 0.5))))
