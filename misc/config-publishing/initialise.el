;;; misc/config-publishing/initialise.el --- common initialisation procudure -*- lexical-binding: t; -*-

(setq start-time (float-time)
      exit-code 0)

(defvar script-root default-directory)
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

(defvar log-messages t)

(defun logged-string (str &optional _term)
  (let ((inhibit-message t)
        (coding-system-for-write 'utf-8))
    (append-to-file str nil log-file)))

(when log-messages
  (advice-add 'send-string-to-terminal :after #'logged-message))

(defvar message-colour t)

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
  (advice-add 'message :around #'timed-coloured-message)
  (advice-add 'doom--print :around #'timed-coloured-message))

;;; Initialisation

(defun initialise (&optional full)
  (advice-add 'theme-magic-from-emacs :override #'ignore)
  (advice-add 'format-all-buffer :override #'ignore)

  (if full
      (load (expand-file-name "~/.emacs.d/init.el"))
    (setq gc-cons-threshold 16777216
          gcmh-high-cons-threshold 16777216)
    (load (expand-file-name "core/core.el" user-emacs-directory) nil t)
    (require 'core-cli)
    (doom-initialize))

  (setq doom-cli-log-error-file log-file)
  (write-region "" nil log-file nil :silent)

  (defalias 'y-or-n-p #'ignore)

  (advice-add 'ask-user-about-supersession-threat :override #'ignore)

  (add-hook! 'doom-debug-mode-hook
    (explain-pause-mode -1))

  (setq emojify-download-emojis-p t)
  (unless (boundp 'image-types) ; why on earth is this needed?
    (setq image-types '(svg png gif tiff jpeg xpm xbm pbm)))

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
Names containing \"*\" are treated as a glob."
  (dolist (file files)
    (if (string-match-p "\\*" file)
        (apply #'publish
               (directory-files (expand-file-name (or (file-name-directory file) "./") config-root)
                                t
                                (dired-glob-regexp (file-name-nondirectory file))))
      (unless (string-match-p "/\\.\\.?$" file)
        (message (concat (when message-colour "[34] ") "Publishing %s") file)
        (let ((target (replace-regexp-in-string (regexp-quote config-root)
                                                publish-dir
                                                (expand-file-name file config-root))))
          (ensure-dir-exists target)
          (copy-file (expand-file-name file config-root) target t))))))
