;; -*- lexical-binding: t; -*-

;; M-x treesit-install-language-grammar to install grammars

;; compile
(use-package popwin
  :ensure t
  :config (progn
            (popwin-mode 1)
            (push '("*compilation*" :height 30) popwin:special-display-config)))

(setq compilation-scroll-output t)

;; rust
(use-package rust-mode
  :ensure t
  :config (require 'rust-compile))

(use-package rust-ts-mode
  :ensure t
  :bind (("C-c C-k" . cargo-transient-check))
  :mode (("\\.rs\\'" . rust-ts-mode)))

;; js
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))

;; cargo
(use-package cargo-transient
  :ensure t)

;; toml
(use-package toml-mode
  :ensure t
  :ensure t)

;; python
(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-ts-mode)))

;; eglot
(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; just
(use-package justl
  :ensure t)

(use-package just-mode
  :ensure t)

;; tmp
(defun cargo-transient-check-x (orig-rust-check)
  "Run chargo check in ~/tmp/cargo."
  (interactive)
  (let* ((envs '("CARGO_TARGET_DIR=/home/aka/tmp/cargo"
                 "CARGO_BUILD_JOBS=12"))
         (process-environment (append envs process-environment)))
    (call-interactively orig-rust-check)))

(advice-add 'cargo-transient-check :around #'cargo-transient-check-x)

;;(make-variable-buffer-local 'compilation-search-path)
;;(push  "/home/aka/devel/gensym/tvbeat/repos/ae/src" compilation-search-path)
