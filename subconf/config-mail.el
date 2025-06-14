;;; config-mail.el --- Generated package (no.59) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.59) from my config.
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

(require 'mu4e)

(defvar mu4e-reindex-request-file "/tmp/mu_reindex_now"
  "Location of the reindex request, signaled by existance")
(defvar mu4e-reindex-request-min-seperation 5.0
  "Don't refresh again until this many second have elapsed.
Prevents a series of redisplays from being called (when set to an appropriate value)")

(defvar mu4e-reindex-request--file-watcher nil)
(defvar mu4e-reindex-request--file-just-deleted nil)
(defvar mu4e-reindex-request--last-time 0)

(defun mu4e-reindex-request--add-watcher ()
  (setq mu4e-reindex-request--file-just-deleted nil)
  (setq mu4e-reindex-request--file-watcher
        (file-notify-add-watch mu4e-reindex-request-file
                               '(change)
                               #'mu4e-file-reindex-request)))

(defadvice! mu4e-stop-watching-for-reindex-request ()
  :after #'mu4e--server-kill
  (if mu4e-reindex-request--file-watcher
      (file-notify-rm-watch mu4e-reindex-request--file-watcher)))

(defadvice! mu4e-watch-for-reindex-request ()
  :after #'mu4e--server-start
  (mu4e-stop-watching-for-reindex-request)
  (when (file-exists-p mu4e-reindex-request-file)
    (delete-file mu4e-reindex-request-file))
  (mu4e-reindex-request--add-watcher))

(defun mu4e-file-reindex-request (event)
  "Act based on the existance of `mu4e-reindex-request-file'"
  (if mu4e-reindex-request--file-just-deleted
      (mu4e-reindex-request--add-watcher)
    (when (equal (nth 1 event) 'created)
      (delete-file mu4e-reindex-request-file)
      (setq mu4e-reindex-request--file-just-deleted t)
      (mu4e-reindex-maybe t))))

(defun mu4e-reindex-maybe (&optional new-request)
  "Run `mu4e--server-index' if it's been more than
`mu4e-reindex-request-min-seperation'seconds since the last request,"
  (let ((time-since-last-request (- (float-time)
                                    mu4e-reindex-request--last-time)))
    (when new-request
      (setq mu4e-reindex-request--last-time (float-time)))
    (if (> time-since-last-request mu4e-reindex-request-min-seperation)
        (mu4e--server-index nil t)
      (when new-request
        (run-at-time (* 1.1 mu4e-reindex-request-min-seperation) nil
                     #'mu4e-reindex-maybe)))))

(setq mu4e-headers-fields
      '((:flags . 6)
        (:account-stripe . 2)
        (:from-or-to . 25)
        (:folder . 10)
        (:recipnum . 2)
        (:subject . 80)
        (:human-date . 8))
      +mu4e-min-header-frame-width 142
      mu4e-headers-date-format "%d/%m/%y"
      mu4e-headers-time-format "⧖ %H:%M"
      mu4e-headers-results-limit 1000
      mu4e-index-cleanup t)

(add-to-list 'mu4e-bookmarks
             '(:name "Yesterday's messages" :query "date:2d..1d" :key ?y) t)

(defvar +mu4e-header--folder-colors nil)
(appendq! mu4e-header-info-custom
          '((:folder .
             (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
              (lambda (msg)
                (+mu4e-colorize-str
                 (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                 '+mu4e-header--folder-colors))))))
(defadvice! +mu4e-personal-address-p--*-a (orig-fn addr)
  :around #'mu4e-personal-address-p
  (or (and (stringp addr)
           (string-match-p "@\\([a-z]+\\.\\)?tecosaur\\.net$" addr))
      (funcall orig-fn addr)))
(setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/evolution.svg")
(custom-set-faces!
  '(mu4e-thread-fold-face :inherit default))
(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
      message-send-mail-function #'message-send-mail-with-sendmail)
(defun mu4e-compose-from-mailto (mailto-string &optional quit-frame-after)
  (require 'mu4e)
  (unless mu4e--server-props (mu4e t) (sleep-for 0.1))
  (let* ((mailto (message-parse-mailto-url mailto-string))
         (to (cadr (assoc "to" mailto)))
         (subject (or (cadr (assoc "subject" mailto)) ""))
         (body (cadr (assoc "body" mailto)))
         (headers (-filter (lambda (spec) (not (-contains-p '("to" "subject" "body") (car spec)))) mailto)))
    (when-let ((mu4e-main (get-buffer mu4e-main-buffer-name)))
      (switch-to-buffer mu4e-main))
    (mu4e~compose-mail to subject headers)
    (when body
      (goto-char (point-min))
      (if (eq major-mode 'org-msg-edit-mode)
          (org-msg-goto-body)
        (mu4e-compose-goto-bottom))
      (insert body))
    (goto-char (point-min))
    (cond ((null to) (search-forward "To: "))
          ((string= "" subject) (search-forward "Subject: "))
          (t (if (eq major-mode 'org-msg-edit-mode)
                 (org-msg-goto-body)
               (mu4e-compose-goto-bottom))))
    (font-lock-ensure)
    (when evil-normal-state-minor-mode
      (evil-append 1))
    (when quit-frame-after
      (add-hook 'kill-buffer-hook
                `(lambda ()
                   (when (eq (selected-frame) ,(selected-frame))
                     (delete-frame)))))))
(defvar mu4e-from-name "Timothy"
  "Name used in \"From:\" template.")
(defadvice! mu4e~draft-from-construct-renamed (orig-fn)
  "Wrap `mu4e~draft-from-construct-renamed' to change the name."
  :around #'mu4e~draft-from-construct
  (let ((user-full-name mu4e-from-name))
    (funcall orig-fn)))
(setq message-signature mu4e-from-name)
(defun +mu4e-update-personal-addresses ()
  (let ((primary-address
         (car (cl-remove-if-not
               (lambda (a) (eq (mod (apply #'* (cl-coerce a 'list)) 600) 0))
               (mu4e-personal-addresses)))))
    (setq +mu4e-personal-addresses
          (and primary-address
               (append (mu4e-personal-addresses)
                       (mapcar
                        (lambda (subalias)
                          (concat subalias "@"
                                  (subst-char-in-string ?@ ?. primary-address)))
                        '("orgmode"))
                       (mapcar
                        (lambda (alias)
                          (replace-regexp-in-string
                           "\\`\\(.*\\)@" alias primary-address t t 1))
                        '("contact" "timothy")))))))

(add-transient-hook! 'mu4e-compose-pre-hook
  (+mu4e-update-personal-addresses))
(defadvice! +mu4e-set-from-adress-h-personal-a (orig-fn)
  :around #'+mu4e-set-from-address-h
  (let* ((msg-addrs
          (and mu4e-compose-parent-message
               (delq nil
                     (mapcar
                      (lambda (adr) (plist-get adr :email))
                      (append (mu4e-message-field mu4e-compose-parent-message :to)
                              (mu4e-message-field mu4e-compose-parent-message :cc)
                              (mu4e-message-field mu4e-compose-parent-message :from))))))
         (personal-addrs
          (if (or mu4e-contexts +mu4e-personal-addresses)
              (and (> (length +mu4e-personal-addresses) 1)
                   +mu4e-personal-addresses)
            (mu4e-personal-addresses)))
         (personal-domain-addr
          (cl-some
           (lambda (email)
             (and (string-match-p "@\\(?:tec\\.\\)?tecosaur\\.net>?$"
                                  email)
                  email))
           msg-addrs)))
    (if (and personal-domain-addr
             (not (cl-intersection msg-addrs personal-addrs :test #'equal)))
        (setq user-mail-address personal-domain-addr)
      (funcall orig-fn))))
(defun +mu4e-account-sent-folder (&optional msg)
  (let ((from (if msg
                  (plist-get (car (plist-get msg :from)) :email)
                (save-restriction
                  (mail-narrow-to-head)
                  (mail-fetch-field "from")))))
    (if (and from (string-match-p "@tecosaur\\.net>?\\'" from))
        "/tecosaur-net/Sent"
      "/sent")))
(setq mu4e-sent-folder #'+mu4e-account-sent-folder)
(defun +mu4e-evil-enter-insert-mode ()
  (when (eq (bound-and-true-p evil-state) 'normal)
    (call-interactively #'evil-append)))

(add-hook 'mu4e-compose-mode-hook #'+mu4e-evil-enter-insert-mode 90)
(defun +mu4e-get-woof-header ()
  (pcase (read-char
          (format "\
%s
  %s Declare %s Applied %s Aborted
%s
  %s Confirm %s Fixed
%s
  %s Request %s Resolved

%s remove X-Woof header"
                  (propertize "Patch" 'face 'outline-3)
                  (propertize "p" 'face '(bold consult-key))
                  (propertize "a" 'face '(bold consult-key))
                  (propertize "c" 'face '(bold consult-key))
                  (propertize "Bug" 'face 'outline-3)
                  (propertize "b" 'face '(bold consult-key))
                  (propertize "f" 'face '(bold consult-key))
                  (propertize "Help" 'face 'outline-3)
                  (propertize "h" 'face '(bold consult-key))
                  (propertize "r" 'face '(bold consult-key))
                  (propertize "x" 'face '(bold error))))
    (?p "X-Woof-Patch: confirmed")
    (?a "X-Woof-Patch: applied")
    (?c "X-Woof-Patch: cancelled")
    (?b "X-Woof-Bug: confirmed")
    (?f "X-Woof-Bug: fixed")
    (?h "X-Woof-Help: confirmed")
    (?r "X-Woof-Help: cancelled")
    (?x 'delete)))
(defun +mu4e-insert-woof-header ()
  "Insert an X-Woof header into the current message."
  (interactive)
  (when-let ((header (+mu4e-get-woof-header)))
    (save-excursion
      (goto-char (point-min))
      (search-forward "--text follows this line--")
      (unless (eq header 'delete)
        (beginning-of-line)
        (insert header "\n")
        (forward-line -1))
      (when (re-search-backward "^X-Woof-" nil t)
        (kill-whole-line)))))

(map! :map mu4e-compose-mode-map
      :localleader
      :desc "Insert X-Woof Header" "w" #'+mu4e-insert-woof-header)

(map! :map org-msg-edit-mode-map
      :after org-msg
      :localleader
      :desc "Insert X-Woof Header" "w" #'+mu4e-insert-woof-header)
(after! mu4e
  (defvar +org-ml-target-dir
    (expand-file-name "lisp/org/" doom-user-dir))
  (defvar +org-ml-max-age 600
    "Maximum permissible age in seconds.")
  (defvar +org-ml--cache-timestamp 0)
  (defvar +org-ml--cache nil)

  (define-minor-mode +org-ml-patchy-mood-mode
    "Apply patches to Org in bulk."
    :global t
    (let ((action (cons "apply patch to org" #'+org-ml-apply-patch)))
      (if +org-ml-patchy-mood-mode
          (add-to-list 'mu4e-view-actions action)
        (setq mu4e-view-actions (delete action mu4e-view-actions)))))

  (defun +org-ml-apply-patch (msg)
    "Apply the patch in the current message to Org."
    (interactive)
    (unless msg (setq msg (mu4e-message-at-point)))
    (with-current-buffer (get-buffer-create "*Shell: Org apply patches*")
      (erase-buffer)
      (let* ((default-directory +org-ml-target-dir)
             (exit-code (call-process "git" nil t nil "am" (mu4e-message-field msg :path))))
        (magit-refresh)
        (when (not (= 0 exit-code))
          (+popup/buffer)))))

  (defun +org-ml-current-patches ()
    "Get the currently open patches, as a list of alists.
Entries of the form (subject . id)."
    (delq nil
          (mapcar
           (lambda (entry)
             (unless (plist-get entry :fixed)
               (cons
                (format "%-8s  %s"
                        (propertize
                         (replace-regexp-in-string "T.*" ""
                                                   (plist-get entry :date))
                         'face 'font-lock-doc-face)
                        (propertize
                         (replace-regexp-in-string "\\[PATCH\\] ?" ""
                                                   (plist-get entry :summary))
                         'face 'font-lock-keyword-face))
                (plist-get entry :id))))
           (with-current-buffer (url-retrieve-synchronously "https://updates.orgmode.org/data/patches")
             (goto-char url-http-end-of-headers)
             (json-parse-buffer :object-type 'plist)))))

  (defun +org-ml-select-patch-thread ()
    "Find and apply a proposed Org patch."
    (interactive)
    (let* ((current-workspace (+workspace-current))
           (patches (progn
                      (when (or (not +org-ml--cache)
                                (> (- (float-time) +org-ml--cache-timestamp)
                                   +org-ml-max-age))
                        (setq +org-ml--cache (+org-ml-current-patches)
                              +org-ml--cache-timestamp (float-time)))
                      +org-ml--cache))
           (msg-id (cdr (assoc (completing-read
                                "Thread: " (mapcar #'car patches))
                               patches))))
      (+workspace-switch +mu4e-workspace-name)
      (mu4e-view-message-with-message-id msg-id)
      (unless +org-ml-patchy-mood-mode
        (add-to-list 'mu4e-view-actions
                     (cons "apply patch to org" #'+org-ml-transient-mu4e-action)))))

  (defun +org-ml-transient-mu4e-action (msg)
    (setq mu4e-view-actions
          (delete (cons "apply patch to org" #'+org-ml-transient-mu4e-action)
                  mu4e-view-actions))
    (+workspace/other)
    (magit-status +org-ml-target-dir)
    (+org-ml-apply-patch msg)))
(after! mu4e
  (defun +mu4e-ml-message-link (msg)
    "Copy the link to MSG on the mailing list archives."
    (let* ((list-addr (or (mu4e-message-field msg :list)
                          (thread-last (append (mu4e-message-field-raw msg :list-post)
                                               (mu4e-message-field msg :to)
                                               (mu4e-message-field msg :cc))
                                       (mapcar (lambda (e) (plist-get e :email)))
                                       (mapcar (lambda (addr)
                                                 (when (string-match-p "emacs.*@gnu\\.org$" addr)
                                                   (replace-regexp-in-string "@" "." addr))))
                                       (delq nil)
                                       (car))))
           (msg-url
            (pcase list-addr
              ("emacs-orgmode.gnu.org"
               (format "https://list.orgmode.org/%s" (mu4e-message-field msg :message-id)))
              (_ (user-error "Mailing list %s not supported" list-addr)))))
      (gui-select-text msg-url)
      (message "Link %s copied to clipboard"
               (propertize msg-url 'face '((:weight normal :underline nil) link)))
      msg-url))

  (add-to-list 'mu4e-view-actions (cons "link to message ML" #'+mu4e-ml-message-link) t))
(defun +browse-url-orgmode-ml (url &optional _)
  "Open an orgmode list url using notmuch."
  (let ((id (and (or (string-match "^https?://orgmode\\.org/list/\\([^/]+\\)" url)
                     (string-match "^https?://list\\.orgmode\\.org/\\([^/]+\\)" url))
                 (match-string 1 url))))
    (mu4e-view-message-with-message-id id)))

(add-to-list 'browse-url-handlers (cons "^https?://orgmode\\.org/list" #'+browse-url-orgmode-ml))
(add-to-list 'browse-url-handlers (cons "^https?://list\\.orgmode\\.org/" #'+browse-url-orgmode-ml))
(defun +mu4e-compose-org-ml-setup ()
  (when (string-match-p "\\`orgmode@" user-mail-address)
    (goto-char (point-min))
    (save-restriction
      (mail-narrow-to-head)
      (when (string-empty-p (mail-fetch-field "to"))
        (re-search-forward "^To: .*$")
        (replace-match "To: emacs-orgmode@gnu.org")
        (advice-add 'message-goto-to :after #'+mu4e-goto-subject-not-to-once)))
    (when (and org-msg-mode
               (re-search-forward "^:alternatives: (\\(utf-8 html\\))" nil t))
      (replace-match "utf-8" t t nil 1))
    (if org-msg-mode
        (let ((final-elem (org-element-at-point (point-max))))
          (when (equal (org-element-property :type final-elem) "signature")
            (goto-char (org-element-property :contents-begin final-elem))
            (delete-region (org-element-property :contents-begin final-elem)
                           (org-element-property :contents-end final-elem))
            (setq-local org-msg-signature
                        (format "\n\n#+begin_signature\n%s\n#+end_signature"
                                (cdr +mu4e-org-ml-signature)))
            (insert (cdr +mu4e-org-ml-signature) "\n")))
      (goto-char (point-max))
      (insert (car +mu4e-org-ml-signature)))
    (setq default-directory
          (file-name-concat doom-user-dir "lisp/org/"))))

(defun +mu4e-goto-subject-not-to-once ()
  (message-goto-subject)
  (advice-remove 'message-goto-to #'+mu4e-goto-subject-not-to-once))
(defvar +mu4e-org-ml-signature
  (cons
   "All the best,
Timothy

-- \
Timothy (‘tecosaur’/‘TEC’), Org mode contributor.
Learn more about Org mode at <https://orgmode.org/>.
Support Org development at <https://liberapay.com/org-mode>,
or support my work at <https://liberapay.com/tec>.
"
   "All the best,\\\\
@@html:<b>@@Timothy@@html:</b>@@

-\u200b- \\\\
Timothy (‘tecosaur’/‘TEC’), Org mode contributor.\\\\
Learn more about Org mode at https://orgmode.org/.\\\\
Support Org development at https://liberapay.com/org-mode,\\\\
or support my work at https://liberapay.com/tec.")
  "Plain and Org version of the org-ml specific signature.")
(add-hook 'mu4e-compose-mode-hook #'+mu4e-compose-org-ml-setup 1)

(setq org-msg-greeting-fmt "\nHi%s,\n\n"
      org-msg-signature "\n\n#+begin_signature\nAll the best,\\\\\n@@html:<b>@@Timothy@@html:</b>@@\n#+end_signature")

(defun +org-msg-goto-body (&optional end)
  "Go to either the beginning or the end of the body.
END can be the symbol top, bottom, or nil to toggle."
  (interactive)
  (let ((initial-pos (point)))
    (org-msg-goto-body)
    (when (or (eq end 'top)
              (and (or (memq initial-pos ; Already at bottom
                             (list (point) (1- (point))))
                       (<= initial-pos ; Above message body
                           (save-excursion
                             (message-goto-body)
                             (point))))
                   (not (eq end 'bottom))))
      (message-goto-body)
      (re-search-forward
       (format (regexp-quote org-msg-greeting-fmt) ; %s is unaffected.
               (concat "\\(?: " (regexp-quote (org-msg-get-to-name)) "\\)?"))))))

(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'+org-msg-goto-body)

(defun +org-msg-goto-body-when-replying (compose-type &rest _)
  "Call `+org-msg-goto-body' when the current message is a reply."
  (when (and org-msg-edit-mode (eq compose-type 'reply))
    (+org-msg-goto-body)))

(advice-add 'mu4e~compose-handler :after #'+org-msg-goto-body-when-replying)

(provide 'config-mail)
;;; config-mail.el ends here
