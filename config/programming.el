;; -*- lexical-binding: t; -*-

;; M-x treesit-install-language-grammar to install grammars

;; popwin
(setq popwin/rules `(("*compilation*" . 30)
                     ("*eldoc*" . 15)))


(defun popwin/apply-rules ()
  (interactive)
  (let ((normalized-rules
         (mapcar (lambda (p)
                   (let ((window (car p))
                         (height (cdr p)))
                     `(,window :height ,height)))
                 popwin/rules)))
    (setq popwin:special-display-config (append normalized-rules popwin/original-value))))

(use-package popwin
  :config (progn
            (popwin-mode 1)
            (setq popwin/original-value popwin:special-display-config)
            (popwin/apply-rules)))

(setq compilation-scroll-output t)

;; rust
(use-package rust-mode
  :config (require 'rust-compile))

(use-package rust-ts-mode
  :bind (("C-c C-k" . rust-check))
  :mode (("\\.rs\\'" . rust-ts-mode)))

;; cargo
;;(use-package cargo-transient)

;; toml
(use-package toml-mode)

;; js
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;; ts
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode))

;; python
(use-package python
  :mode (("\\.py\\'" . python-ts-mode)))

;; julia
;; (use-package julia-mode
;;   :mode(("\\.jl\\`" . julia-mode))) ;;; julia-ts-mode

;; (use-package eglot-jl
;;   :hook ((julia-mode . eglot-jl-init)))

;; (use-package julia-vterm)

;; eglot
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         ;;;;(julia-mode . elgot-ensure)
         )
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; just
(use-package justl)
(use-package just-mode)

;; tmp
;; (defun cargo-transient-check-x (orig-rust-check)
;;   "Run chargo check in ~/tmp/cargo."
;;   (interactive)
;;   (let* ((envs '("CARGO_TARGET_DIR=/home/aka/tmp/cargo"
;;                  "CARGO_BUILD_JOBS=12"))
;;          (process-environment (append envs process-environment)))
;;     (call-interactively orig-rust-check)))

;; (advice-add 'cargo-transient-check :around #'cargo-transient-check-x)

;;(make-variable-buffer-local 'compilation-search-path)
;;(push  "/home/aka/devel/gensym/tvbeat/repos/ae/src" compilation-search-path)
