;; -*- lexical-binding: t; -*-

;; projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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

(use-package toml-mode
  :ensure t)

(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)
         (toml-mode . cargo-minor-mode)))

;; js
;;
;; npm install -g --save typescript
;; npm install -g typescript-language-server
;;

(add-hook 'js-mode-hook #'lsp)

(use-package js-comint
  :ensure t)

(use-package json-mode
  :ensure t)
