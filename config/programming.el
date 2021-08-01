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

;;
(use-package eglot
  :custom (tab-width 4))

(defun find-projectile-project (dir)
  (let ((root (locate-dominating-file dir ".projectile")))
    (when root `(vc . ,root))))

(add-hook 'project-find-functions 'find-projectile-project)

;;
(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

(use-package toml-mode)
