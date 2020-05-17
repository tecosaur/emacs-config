;;; wttrin.el --- Emacs frontend for weather web service wttr.in -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Carl X. Su

;; Author: Carl X. Su <bcbcarl@gmail.com>
;;         ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4") (xterm-color "1.0"))
;; Keywords: comm, weather, wttrin
;; URL: https://github.com/bcbcarl/emacs-wttrin
;;
;; Modifications made by @tecosaur

;;; Commentary:

;; Provides the weather information from wttr.in based on your query condition.

;;; Code:

(require 'url)
(require 'xterm-color)

(defgroup wttrin nil
  "Emacs frontend for weather web service wttr.in."
  :prefix "wttrin-"
  :group 'comm)

(defcustom wttrin-default-api-version 1
  "Specifies which version of the wttrin API to use."
  :group 'wttrn
  :type '(choice (const 1) (const 2)))

(defcustom wttrin-default-cities '("Baghdad"
                                   "Beijing"
                                   "Brussels"
                                   "Buenos Aires"
                                   "Cairo"
                                   "Delhi"
                                   "Gurnsey"
                                   "Ho Chi Ming City"
                                   "Hong Kong"
                                   "Istanbul"
                                   "Johannesburg"
                                   "Kuala Lumpur"
                                   "Leipzig"
                                   "Lima"
                                   "London"
                                   "Madrid"
                                   "Manila"
                                   "Mexico City"
                                   "Miami"
                                   "Moscow"
                                   "Mumbai"
                                   "New York"
                                   "Paris"
                                   "Seoul"
                                   "Shanghai"
                                   "Singapore"
                                   "Surat"
                                   "Sydney"
                                   "Tokyo"
                                   "Toronto"
                                   ;; and the fun one!
                                   "Moon")
  "Specify default cities list for quick completion."
  :group 'wttrin
  :type 'list)

(defcustom wttrin-default-accept-language '("Accept-Language" . "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4")
  "Specify default HTTP request Header for Accept-Language."
  :group 'wttrin
  :type '(list)
  )

(defun wttrin-fetch-raw-string (query &optional api-version)
  "Get the weather information based on your QUERY."
  (unless api-version (setq api-version wttrin-default-api-version))
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://v" (number-to-string api-version) ".wttr.in/" query)
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

(defun wttrin-exit ()
  (interactive)
  (quit-window t))

(defun wttrin-query (city-name &optional api-version)
  "Query weather of CITY-NAME via wttrin, and display the result in new buffer."
  (let ((raw-string (wttrin-fetch-raw-string city-name api-version)))
    (if (string-match "ERROR" raw-string)
        (message "Cannot get weather data. Maybe you inputed a wrong city name?")
      (let ((buffer (get-buffer-create (format "*wttr.in - %s*" city-name))))
        (switch-to-buffer buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (xterm-color-filter raw-string))
        (goto-char (point-min))
        (save-excursion
          (re-search-forward "^$")
          (delete-region (point-min) (1+ (point))))
        (save-excursion
          (while  (re-search-forward "(B" nil t)
            (delete-region (match-beginning 0) (match-end 0))))
        (use-local-map (make-sparse-keymap))
        (local-set-key "q" 'wttrin-exit)
        (local-set-key "g" 'wttrin)
        (setq buffer-read-only t)))))

;;;###autoload
(defun wttrin (city &optional api-version)
  "Display weather information for CITY."
  (interactive
   (cond ((equal current-prefix-arg nil)
          (list "" nil))
         ((equal current-prefix-arg 1)
          (list "" 1))
         ((equal current-prefix-arg 2)
          (list "" 2))
         (t (list
             (completing-read "City name: " wttrin-default-cities nil nil
                              (when (= (length wttrin-default-cities) 1)
                                (car wttrin-default-cities)))))))
  (wttrin-query city api-version))

(provide 'wttrin)

;;; wttrin.el ends here
