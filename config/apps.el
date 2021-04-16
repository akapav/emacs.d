;; -*- lexical-binding: t; -*-

;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn
            (setq org-startup-indented t)
            (setq org-hide-leading-stars t)))

;; dired
;; (require 'dired-x)

;; (use-package dired-x
;;   :init   (setq dired-dwim-target t)
;;   :config (progn
;;             (setq dired-omit-files "^\\...+$")
;;             (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;;             (add-hook 'dired-mode-hook 'auto-revert-mode)))

;; magit
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; erc
;;; TODO(aka): remove a startup warning
;;;(use-package znc :straight t)
