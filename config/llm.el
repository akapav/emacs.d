;; -*- lexical-binding: t; -*-

;; gptel: interactive chat and rewriting
;; C-c g     open/send to gptel buffer (also switches model/backend)
;; C-c RET   send in an active gptel buffer
;; C-c r     rewrite selected region (gptel-rewrite)
(use-package gptel
  :ensure t
  :bind (("C-c g" . gptel-menu)
         ("C-c r" . gptel-rewrite))
  :config
  ;; Local Ollama — update model list as you install more (ollama list)
  (defvar llm/ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(qwen2.5-coder:32b deepseek-coder-v2:latest)))

  ;; Google Gemini
  (defvar llm/gemini
    (gptel-make-gemini "Gemini"
      :key (lambda ()
             (auth-source-pick-first-password
              :host "generativelanguage.googleapis.com"
              :user "apikey"))
      :stream t
      :models '(gemini-3.1-pro-preview gemini-3-flash-preview)))

  ;; Anthropic Claude
  (defvar llm/claude
    (gptel-make-anthropic "Claude"
      :key (lambda ()
             (auth-source-pick-first-password
              :host "api.anthropic.com"))
      :stream t
      :models '(claude-opus-4-6 claude-sonnet-4-6 claude-haiku-4-5-20251001)))

  ;; Default to local Ollama; switch via C-c g
  (setq-default gptel-backend llm/ollama
                gptel-model   'qwen2.5-coder:32b)
  (setq gptel-default-mode 'org-mode))

;; aidermacs: full agent / aider mode
;; C-c A     open aidermacs transient menu
(use-package aidermacs
  :ensure t
  :bind ("C-c A" . aidermacs-transient-menu)
  :config
  (setenv "GEMINI_API_KEY"
          (auth-source-pick-first-password
           :host "generativelanguage.googleapis.com"
           :user "apikey"))
  (setq aidermacs-default-model "gemini/gemini-2.5-flash"
        aidermacs-extra-args    '("--no-auto-commits")
        aidermacs-backend       'vterm))
