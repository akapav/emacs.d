;; -*- lexical-binding: t; -*-

;; compile
(use-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*Cargo Check*" :height 30) popwin:special-display-config)))
(setq compilation-scroll-output t)

;; company
(use-package company
  :init (global-company-mode))

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; lsp
(use-package lsp-mode
  :commands lsp)

;; rust
(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package toml-mode)

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)
         (toml-mode . cargo-minor-mode)))

;; js
;;
;; npm install -g --save typescript
;; npm install -g typescript-language-server
;;
(add-hook 'js-mode-hook #'lsp)
(use-package js-comint)
(use-package json-mode)
