;; -*- lexical-binding: t; -*-

;; projectile
(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;;(use-package counsel-projectile
;;;  :straight t)

;; compile
(use-package popwin
  :straight t
  :config (progn
            (popwin-mode 1)
            (push '("*Cargo Check*" :height 20) popwin:special-display-config)))
(setq compilation-scroll-output t)

;; company
(use-package company
  :straight t
  :init (global-company-mode))

;; lsp
(use-package lsp-mode
  :straight t
  :commands lsp)

(use-package company-lsp
  :straight t
  :commands company-lsp)

;; rust
(use-package rust-mode
  :straight t
  :hook (rust-mode . lsp))

(use-package toml-mode
  :straight t)

(use-package cargo
  :straight t
  :hook ((rust-mode . cargo-minor-mode)
         (toml-mode . cargo-minor-mode)))

;; js
;;
;; npm install -g --save typescript
;; npm install -g typescript-language-server
;;

(add-hook 'js-mode-hook #'lsp)

(use-package js-comint
  :straight t)

(use-package json-mode
  :straight t)
