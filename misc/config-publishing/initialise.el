;;; misc/config-publishing/initialise.el --- common initialisation procudure -*- lexical-binding: t; -*-

(setq start-time (float-time)
      exit-code 0)

(defvar config-root (file-name-directory ; $DOOM_DIR/
                     (directory-file-name
                      (file-name-directory ; $DOOM_DIR/misc
                       (directory-file-name
                        (file-name-directory load-file-name))))))

;;; Report errors

(setq debug-on-error t
      doom-debug-p t)

(defvar log-file "unnamed-log.txt")

(write-region "" nil log-file)

;;; Messaging

(defvar message-colour t)

(defun logged-message (msg)
  (unless inhibit-message
    (let ((inhibit-message t)
          (coding-system-for-write 'utf-8))
      (princ #'external-debugging-output #'ignore)
      (append-to-file msg nil log-file)
      (append-to-file "\n" nil log-file)))
  msg)

(advice-add 'message :filter-return #'logged-message)

(defun red-error (orig-fn &rest args)
  (message "\033[0;31m" 'unmodified)
  (apply orig-fn args)
  (message "\033[0m" 'unmodified)
  (setq exit-code 1))

(defun timed-coloured-message (orig-fn format-str &rest args)
  (cond
   ((eq (car args) 'unmodified)
    (apply orig-fn format-str (cdr args)))
   ((or (not format-str) (string-match-p "\\[%\\\\4.1fs\\]" format-str))
    (apply orig-fn format-str args))
   (t
    (apply orig-fn
           (concat (if (string-match-p "^\\[[0-9;]+\\]" format-str)
                       (replace-regexp-in-string
                        "^\\(?:\\[\\([0-9;]+\\)\\] ?\\)?"
                        "\033[\\1m[%4.1fs] "
                        format-str)
                     (concat "[%4.1fs] " format-str))
                   "\033[0;90m")
           (append (list (- (float-time) start-time))
                   args)))))

(when message-colour
  (advice-add 'debug :around #'red-error)
  (advice-add 'message :around #'timed-coloured-message))

;;; Initialisation

(defun initialise (&optional full)
  (advice-add 'theme-magic-from-emacs :override #'ignore)
  (advice-add 'format-all-buffer :override #'ignore)

  ;; Avoid error: file-missing "Opening directory" "No such file or directory" "~/org/roam"
  (setq org-roam-directory "~")
  (advice-add 'org-roam-mode :override #'ignore)
  (advice-add 'org-roam-db-build-cache :override #'ignore)

  (if full
      (load (expand-file-name "~/.emacs.d/init.el"))
    (setq gc-cons-threshold 16777216
          gcmh-high-cons-threshold 16777216)
    (load (expand-file-name "core/core.el" user-emacs-directory) nil t)
    (require 'core-cli)
    (doom-initialize))

  (defalias 'y-or-n-p #'ignore)

  (advice-add 'ask-user-about-supersession-threat :override #'ignore)

  (after! undo-tree
    (global-undo-tree-mode -1)
    (advice-add 'undo-tree-mode :override #'ignore)
    (remove-hook 'write-file-functions #'undo-tree-save-history-from-hook)
    (remove-hook 'kill-buffer-hook #'undo-tree-save-history-from-hook)
    (remove-hook 'find-file-hook #'undo-tree-load-history-from-hook)))

;;; Publishing

(defvar publish-dir (expand-file-name "publish/" config-root))

(defvar known-existing-dirs (list config-root))
(defun ensure-dir-exists (file-or-dir)
  (let ((dir (file-name-directory (expand-file-name file-or-dir config-root))))
    (unless (member dir known-existing-dirs)
      (unless (file-exists-p dir)
        (make-directory dir t))
      (push dir known-existing-dirs))))

(defun publish (&rest files)
  "Move each file into `publish'.
Names containing \"*\" are treate as a glob."
  (dolist (file files)
    (if (string-match-p "\\*" file)
        (apply #'publish
               (directory-files (expand-file-name (or (file-name-directory file) "./") config-root)
                                t
                                (dired-glob-regexp (file-name-nondirectory file))))
      (message (concat (when message-colour "[34] ") "Publishing %s") file)
      (let ((target (replace-regexp-in-string (regexp-quote config-root)
                                              publish-dir
                                              (expand-file-name file config-root))))
        (ensure-dir-exists target)
        (copy-file (expand-file-name file config-root) target t)))))
