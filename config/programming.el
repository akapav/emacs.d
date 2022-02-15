;; -*- lexical-binding: t; -*-

;; project
(use-package project)

;; compile
(use-package popwin
  :config (progn
            (popwin-mode 1)
            (push '("*compilation*" :height 30) popwin:special-display-config)))
(setq compilation-scroll-output t)

;; company
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))

;; rustic
(use-package flycheck)
(use-package lsp-mode
  :config
  (with-eval-after-load 'lsp-rust
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection (lambda () lsp-rust-rls-server-command))
                      :major-modes '(rust-mode rustic-mode)
                      :remote? t
                      :priority (if (eq lsp-rust-server 'rls) 1 -1)
                      :initialization-options '((omitInitBuild . t)
                                                (cmdRun . t))
                      :notification-handlers (ht ("window/progress" 'lsp-clients--rust-window-progress))
                      :action-handlers (ht ("rls.run" 'lsp-rust--rls-run))
                      :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
                      :initialized-fn (lambda (workspace)
                                        (with-lsp-workspace workspace
                                          (lsp--set-configuration
                                           (lsp-configuration-section "rust"))))
                      :server-id 'rls))))

(use-package rustic)




(use-package toml-mode)
