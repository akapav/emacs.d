;; -*- lexical-binding: t; -*-

;; personal information
(setq
 user-full-name "alan pavičić"
 user-mail-address "aka@gensym.net"

 auth-sources
 '((:source "~/.emacs.d/secrets/.authinfo.gpg")))

;; basic bahaviour
(setq
 warning-minimum-level :emergency
 select-enable-clipboard t
 backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; suspend -> repeat
(put 'suspend-frame 'disabled t)
(global-set-key [(control z)] 'repeat)

;; customizations
(setq custom-file "~/.emacs.d/cust.el")
(load custom-file t)

;; macos
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (eq system-type 'darwin)
    (setq ns-use-native-fullscreen nil)
    (exec-path-from-shell-initialize)))

;; appereance
(setq inhibit-splash-screen t
      visible-bell nil
      ring-bell-function (lambda nil))

(unless (eq system-type 'darwin)
  (menu-bar-mode 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(fringe-mode 1)
(global-hl-line-mode 1)
(mouse-wheel-mode t)
(load-theme 'modus-operandi)

;; font
(defun set-font ()
  (message "set font")
  (set-frame-font "Jetbrains Mono NL-13")
  (setq-default line-spacing 0.15))

(set-font)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame (set-font)))))

;; highlight selected window
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (let ((selected-buffer (window-buffer (selected-window))))
    (walk-windows (lambda (w)
                    (let ((buff (window-buffer w)))
                      (let ((bg
                             (if (eq selected-buffer buff)
                                 'default
                               `(:background
                                 ,(face-attribute
                                   'mode-line-inactive :background)))))
                        (with-current-buffer buff
                          (buffer-face-set bg))))))))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)

;; buffers
(global-set-key [(control x) (control b)] 'ibuffer)

;; scratch buffer
(defun scratch-buffer ()
  "Show or create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; org scratch buffer
(defun org-scratch-buffer ()
  "Show or create an org scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*org scratch*"))
  (org-mode))

;; caret
(defun set-cursor-according-to-mode ()
 (cond
   (buffer-read-only
     (set-cursor-color "grey"))
   (overwrite-mode
     (set-cursor-color "red"))
   (t
     (set-cursor-color "black"))))

;; (add-hook 'post-command-hook 'set-cursor-according-to-mode)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 3)

;; parenthesis
(show-paren-mode 1)
(customize-set-variable 'show-paren-style 'mixed)

;; line numbers
(defun goto-line-x (orig-goto-line)
  "Display line number on interactive goto-line."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (call-interactively orig-goto-line))
    (display-line-numbers-mode -1)))

(advice-add 'goto-line :around #'goto-line-x)

;;window move
(global-set-key [(control tab)] 'other-window)

;;
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package windmove
  :ensure t
  :config (windmove-default-keybindings 'meta))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode))

;;
(defun apply-region-or-line (fn)
  (let ((bounds
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-end-position)))))
    (apply fn bounds)))

(defun kill-ring-save-x ()
  "Copy region or line."
  (interactive)
  (apply-region-or-line #'kill-ring-save))

(defun kill-region-x ()
  "Cut region or line."
  (interactive)
  (apply-region-or-line #'kill-region))

(define-key (current-global-map) [remap kill-ring-save] 'kill-ring-save-x)
(define-key (current-global-map) [remap kill-region   ] 'kill-region-x   )

(use-package visible-mark
  :ensure t
  :config (global-visible-mark-mode 1))

;; yank/kill
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

(defun kill-eol-save ()
  "Save to the end of line as if killed, but don't kill it."
  (interactive)
  (let ((end (save-excursion (end-of-visible-line) (point))))
    (kill-ring-save (point) end)))

(define-key (current-global-map) [remap kill-sentence] 'kill-eol-save)

(delete-selection-mode 1)

;; whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace
  :ensure t
  :init   (setq
           whitespace-style            '(trailing tabs tab-mark face)
           whitespace-global-modes     '(not erc-mode))
  :config (global-whitespace-mode))

(defun untabify-x (orig-untabify &rest -)
  "Untabify region or line."
  (interactive)
  (apply-region-or-line orig-untabify))

(advice-add 'untabify :around #'untabify-x)

;; navigation
(defun move-beginning-of-line-x ()
  "Toggle beginning of a line and beginning of a indentation."
  (interactive)
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(define-key (current-global-map)
  [remap move-beginning-of-line] 'move-beginning-of-line-x)

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(global-set-key [(control shift w)] 'electric-buffer-list)

;; breadcrumb
(use-package breadcrumb
  :config (breadcrumb-mode 1))

;; which key
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; ripgrep
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

;; company
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

;;completions/vertico/orgerless/marginalia
(use-package vertico
  :ensure t
 :init (vertico-mode))

(use-package vertico-directory
  :ensure t
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(partial-completion orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; add to :custom in orderless
;; basic completion style for tramp
;; (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; hydra
(use-package hydra
  :ensure t
  :init
  (progn
    (defhydra hydra-navigate (:hint nil)
"
^Buffer^            ^Page^              ^Function^
^^^^^^^^----------------------------------------------------
_3_: beginning      _e_: up             _d_: up
_4_: end            _r_: down           _f_: down
"
      ("3" beginning-of-buffer)
      ("4" end-of-buffer)
      ("e" scroll-down)
      ("r" scroll-up)
      ("d" beginning-of-defun)
      ("f" end-of-defun)
      ("c" nil "cancel" :color red)))
  :bind ("M-j" . hydra-navigate/body))

;; TODO: consult

;; TODO: abbrew support
;; (global-set-key [select] 'dabbrev-expand)
;; (global-set-key [select] 'hippie-expand)

;; org-mode
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn
            (require 'ox-md)
            (setq org-startup-indented t)
            (setq org-hide-leading-stars t)
            (setq org-cycle-separator-lines 0)))

;; org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config (org-roam-db-autosync-enable))

;; dired
(use-package dired-x
  :ensure t
  :straight nil
  :bind (("C-c C-h" . dired-omit-mode))
  :config (progn
            (setq dired-omit-files "\\`[.][^.].*\\'")
            (setq dired-listing-switches "-alh")
            (setq dired-dwim-target t)
            (put 'dired-find-alternate-file 'disabled nil))
  :hook ((dired-mode . dired-omit-mode)))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package
  :ensure t
  forge :after magit)


;; eshell
(use-package eshell
  :ensure t
  :init
  (progn
    (require 'em-smart)
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t))
  :hook (eshell-mode . eshell-smart-initialize))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; chatgpt
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda () (auth-source-pick-first-password :host "api.openai.com")))))

;; erc
;;; TODO(aka): remove a startup warning
;;;(use-package znc :straight t)

(use-package erc
  :ensure t
  :init
  (progn
    (setq erc-autojoin-channels-alist
          '(("libera.chat" "#razmjenavjestina")))
    (setq erc-prompt-for-nickserv-password nil)))

(defun erc/connect ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :port 6697 :nick "akapav"))


;; vterm
(use-package vterm
  :hook ((vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))))


;;;(when (daemonp) (erc/connect))

;; tramp
;; (use-package tramp
;;   :init
;;   (autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
;;   :config
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; gemini
;; (use-package elpher)
;; (use-package gemini-mode)
