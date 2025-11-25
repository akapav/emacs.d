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

(defun rust-tree-sitter-current-function-name ()
  "Return the name of the Rust function around point using tree-sitter."
  (when (and (eq major-mode 'rust-ts-mode)
             (treesit-ready-p 'rust))
    (save-excursion
      (let ((node (treesit-node-at (point))))
        (while (and node
                    (not (equal (treesit-node-type node) "function_item")))
          (setq node (treesit-node-parent node)))
        (when node
          (let ((name-node (treesit-node-child-by-field-name node "name")))
            (when name-node
              (treesit-node-text name-node))))))))


(defun rust-run-test-at-point (arg)
  "Run `cargo test` for the Rust function at point using Tree-sitter."
  (interactive "P")
  (let ((fn-name (rust-tree-sitter-current-function-name))
        (capture-flag (if arg "-- --nocapture" "")))
    (if fn-name
        (compile (format "cargo test --release  %s %s" capture-flag fn-name))
      (message "No Rust function found at point."))))

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

(use-package pyvenv) ;;; activate .venv from emacs

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

;; codex
(defun codex-login ()
  (interactive)
  (let* ((entry (car (auth-source-search :host "api.openai.com" :max 1)))
         (secret (when entry
                   (let ((s (plist-get entry :secret)))
                     (if (functionp s) (funcall s) s)))))
    (if (not secret)
        (message "API key not found for api.openai.com")
      (with-temp-buffer
        (insert secret)
        (let ((exit-code
               (call-process-region
                (point-min) (point-max)
                "codex"         ; program
                nil             ; don't delete input
                "*Codex Login*" ; output buffer
                nil             ; display asynch? nil==wait
                "login" "--with-api-key")))
          (if (zerop exit-code)
              (progn (with-current-buffer "*Codex Login*"
                       (message "codex login successful: %s"
                                (buffer-string))))
            (progn
              (pop-to-buffer "*Codex Login*")
              (message "codex login failed (code %d)" exit-code))))))))

(defun codex-shell ()
  (interactive)
  (eat "codex"))
