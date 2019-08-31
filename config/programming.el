;; -*- lexical-binding: t; -*-

;; compile
(setq compilation-scroll-output t)

;; company
(use-package company
  :ensure t
  :init (global-company-mode))

;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (progn
            (setq lsp-enable-snippet nil)
            (require 'lsp-clients)))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; rust
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))
