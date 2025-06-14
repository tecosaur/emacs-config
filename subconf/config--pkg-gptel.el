;;; config--pkg-gptel.el --- Generated package (no.19) from my config -*- lexical-binding: t; -*-
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
;;  Generated package (no.19) from my config.
;;
;;  During generation, dependency on other aspects of my configuration and
;;  packages is inferred via (regexp-based) static analysis.  While this seems
;;  to do a good job, this method is imperfect.  This code likely depends on
;;  utilities provided by Doom, and if you try to run it in isolation you may
;;  discover the code makes more assumptions.
;;
;;  That said, I've found pretty good results so far.
;;
;;  Package statement:
;;   (package! gptel :pin "94bf19da93aee9a101429d7ecbfbb9c7c5b67216")
;;
;;; Code:


(use-package! gptel
  :commands gptel gptel-menu gptel-mode gptel-send
  :config
  (let ((groq-backend
         (gptel-make-openai "Groq"
           :host "api.groq.com"
           :endpoint "/openai/v1/chat/completions"
           :stream t
           :key (lambda () (or (secrets-get-secret "Login" "groq")
                          (secrets-get-secret "kdewallet" "groq")))
           :models '("llama3-70b-8192"
                     "llama3-8b-8192"
                     "llama-3.1-70b-versatile"
                     "llama-3.1-8b-instant"
                     "llama-3.2-1b-preview"
                     "deepseek-r1-distill-llama-70b"
                     "mixtral-8x7b-32768"
                     "gemma-7b-it"
                     "gemma2-9b-it")))
        (openai-backend
         (gptel-make-openai "ChatGPT"
           :host "api.openai.com"
           :stream t
           :key (lambda () (or (secrets-get-secret "Login" "openai")
                          (secrets-get-secret "kdewallet" "openai")))
           :models '("gpt-4o" "gpt-4o-mini" "chatgpt-4o-latest"
                     "o1" "o1-mini")))
        (anthropic-backend
         (gptel-make-anthropic "Claude"
           :stream t
           :key (lambda () (or (secrets-get-secret "Login" "anthropic")
                          (secrets-get-secret "kdewallet" "anthropic")))
           :models '("claude-3-5-sonnet-20240620"
                     "claude-3-sonnet-20240229"
                     "claude-3-haiku-20240307")))
        (ollama-backend
         (let (ollama-models)
           (when (executable-find "ollama")
             (with-temp-buffer
               (call-process "ollama" nil t nil "list")
               (goto-char (point-min))
               (forward-line 1)
               (while (and (not (eobp)) (looking-at "[^ \t]+"))
                 (push (match-string 0) ollama-models)
                 (forward-line 1)))
             (gptel-make-ollama "Ollama" :models ollama-models :stream t)))))
    (setq-default gptel-model "llama-3.1-70b-versatile"
                  gptel-backend groq-backend))
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends)
  (setq gptel-default-mode #'org-mode))

(provide 'config--pkg-gptel)
;;; config--pkg-gptel.el ends here
