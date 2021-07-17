;; -*- lexical-binding: t; -*-

;; compile
(use-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*compilation*" :height 30) popwin:special-display-config)))
(setq compilation-scroll-output t)

;; company
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package eglot)

(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

(use-package toml-mode)

;;(use-package cargo
;;  :hook ((rust-mode . cargo-minor-mode)
;;         (toml-mode . cargo-minor-mode)))

;; js
;;
;; npm install -g --save typescript
;; npm install -g typescript-language-server
;;
;;(add-hook 'js-mode-hook #'lsp)
;;(use-package js-comint)
;;(use-package json-mode)
