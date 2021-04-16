;; -*- lexical-binding: t; -*-

;; customizations
(setq custom-file "~/.emacs.d/cust.el")
(load custom-file t)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; basic appearance
(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(fringe-mode 1)

;; theme
;;(load-theme 'tango)
(use-package modus-themes
  :straight t
  :init (modus-themes-load-operandi))

;; font
(set-frame-font "Jetbrains Mono-13")
(setq-default line-spacing 0.2)

;; bell
(setq visible-bell nil
      ring-bell-function (lambda nil))

;; x clipboard
(setq select-enable-clipboard t)

;; mouse scroll
(mouse-wheel-mode t)

;; zoom in/out
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; suspend -> repeat
(put 'suspend-frame 'disabled t)
(global-set-key [(control z)] 'repeat)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; scratch buffer
(defun scratch-buffer ()
  "Show or create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; carret
(defun set-cursor-according-to-mode ()
 (cond
   (buffer-read-only
     (set-cursor-color "grey"))
   (overwrite-mode
     (set-cursor-color "red"))
   (t
     (set-cursor-color "black"))))

(add-hook 'post-command-hook 'set-cursor-according-to-mode)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 3)

;; line numbers
(defun goto-line-x (orig-goto-line)
  "Display line number on interactive goto-line."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively orig-goto-line))
    (linum-mode -1)))

(advice-add 'goto-line :around #'goto-line-x)

;;window move
(global-set-key [(control tab)] 'other-window)

(use-package windmove
  :straight t
  :config (windmove-default-keybindings 'meta))

(use-package window-numbering
  :straight t
  :config (window-numbering-mode))

;;
(defun apply-region-or-line (fn)
  (let ((bounds
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-end-position)))))
    (apply fn bounds)))

;; yank/kill
(use-package browse-kill-ring
  :straight t
  :bind (("M-y" . browse-kill-ring)))

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

(delete-selection-mode 1)

;; whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace
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
  :straight t
  :bind (("M-s" . avy-goto-word-1)))

(global-set-key [(control shift w)] 'electric-buffer-list)

;; abbrew
;;; todo(aka)
(global-set-key [select] 'dabbrev-expand)
;;;(global-set-key [select] 'hippie-expand)

;; which key
(use-package which-key
  :straight t
  :config (which-key-mode t))

;; paren
(show-paren-mode t)

;; ;; ido
;; (use-package ido
;;   :init (progn
;;           (setq ido-enable-flex-matching t)
;;           (setq ido-everywhere t)
;;           (ido-mode t)))

;; smex
;; smex is used by counsel for lru
(use-package smex
 :straight t
 :config (smex-initialize))

(use-package ivy
  :straight t
  :init (ivy-mode))

(use-package counsel
  :straight t
  :init (counsel-mode)
  :bind (("M-x" . counsel-M-x)))

;; grep
;; TODO(aka): counsel-rg
(use-package rg
  :straight t
  :config (rg-enable-default-bindings))

;; can comment, dependency of rg
;;(use-package wgrep
;;  :straight t)
