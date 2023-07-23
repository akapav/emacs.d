;; -*- lexical-binding: t; -*-

;; tree sitter
;; use script here to fetch languages definitions
;; git@github.com:casouri/tree-sitter-module.git
(setq treesit-extra-load-path '("~/devel/3rd/tree-sitter-module/dist/"))

;; compile
(use-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*compilation*" :height 30) popwin:special-display-config)))

(setq compilation-scroll-output t)

;; rust
(use-package rust-mode)

;;

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config (progn
            (add-to-list 'eglot-server-programs
             '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

            (defclass eglot-deno (eglot-lsp-server) ()
              :documentation "A custom class for deno lsp.")

            (cl-defmethod eglot-initialization-options ((server eglot-deno))
              "Passes through required deno initialization options"
              (list :enable t :lint t))))

;;
(defun rust-check-x (orig-rust-check)
  "Run chargo check in /tmp/cargo."
  (interactive)
  (let* ((envs '("CARGO_TARGET_DIR=/home/aka/tmp/cargo"
                 "CARGO_BUILD_JOBS=12"))
         (process-environment (append envs process-environment)))
    (call-interactively orig-rust-check)))

(advice-add 'rust-check :around #'rust-check-x)

;;(make-variable-buffer-local 'compilation-search-path)
;;(push  "/home/aka/devel/gensym/tvbeat/repos/ae/src" compilation-search-path)


(use-package typescript-mode)

(use-package toml-mode)
