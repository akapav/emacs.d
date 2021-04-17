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
(add-hook 'dired-mode-hook (lambda ()
                             (require 'dired-x)
                             (setq dired-omit-files "\\`[.][^.].*\\'")
                             (setq dired-dwim-target t)
                             (dired-omit-mode 1)))

;; magit
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; erc
;;; TODO(aka): remove a startup warning
;;;(use-package znc :straight t)
