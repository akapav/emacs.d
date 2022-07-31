;; -*- lexical-binding: t; -*-

;; project
(use-package project)

;; compile
(use-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*compilation*" :height 30) popwin:special-display-config)))
(setq compilation-scroll-output t)

;; rust
(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

(use-package eglot)

(defun rust-check-x (orig-rust-check)
  "Run chargo check in /tmp/cargo."
  (interactive)
  (let ((process-environment (cons "CARGO_TARGET_DIR=/tmp/cargo" process-environment)))
    (call-interactively orig-rust-check)))

(advice-add 'rust-check :around #'rust-check-x)


(use-package toml-mode)
