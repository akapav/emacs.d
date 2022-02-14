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
(use-package dired-x
  :straight nil
  :bind (("C-c C-h" . dired-omit-mode))
  :config (progn
            (setq dired-omit-files "\\`[.][^.].*\\'")
            (setq dired-dwim-target t)
            (put 'dired-find-alternate-file 'disabled nil))
  :hook ((dired-mode . dired-omit-mode)))

;; magit
(use-package magit
  :bind (("C-x g" . magit-status)))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; erc
;;; TODO(aka): remove a startup warning
;;;(use-package znc :straight t)

(use-package erc
  :init
  (progn
    (setq erc-autojoin-channels-alist
          '(("libera.chat" "#razmjenavjestina")))
    (setq erc-prompt-for-nickserv-password nil)))

(defun erc/connect ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697 :nick "akapav"))

(when (daemonp) (erc/connect))

;; tramp
(use-package tramp
  :init
  (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; gemini
(use-package elpher)
(use-package gemini-mode)
