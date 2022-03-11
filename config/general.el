;; -*- lexical-binding: t; -*-

;; auth
(setq auth-sources
      '((:source "~/.emacs.d/secrets/.authinfo.gpg")))

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
(use-package modus-themes
  :init (modus-themes-load-operandi))

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (let ((selected-buffer (window-buffer (selected-window))))
    (walk-windows (lambda (w)
                    (let ((buff (window-buffer w)))
                      (let ((bg
                             (if (eq selected-buffer buff)
                                 'default
                               '(:background "#eee"))))
                          (with-current-buffer buff
                            (buffer-face-set bg))))))))

(add-hook 'buffer-list-update-hook 'highlight-selected-window)
(global-hl-line-mode 1)

;; font
(defun set-font ()
  (message "set font")
  (set-frame-font "Jetbrains Mono-13")
  (setq-default line-spacing 0.2))

(set-font)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (with-selected-frame frame (set-font)))))

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

;; parenthesis
(show-paren-mode 1)
(customize-set-variable 'show-paren-style 'expression)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;(add-hook 'post-command-hook 'set-cursor-according-to-mode)
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
  :config (windmove-default-keybindings 'meta))

(use-package window-numbering
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
  :bind (("M-s" . avy-goto-word-1)))

(global-set-key [(control shift w)] 'electric-buffer-list)

;; which key
(use-package which-key
  :config (which-key-mode t))

;; ripgrep
(use-package rg
  :config (rg-enable-default-bindings))

;;completions/vertico/orgerless/marginalia
(use-package vertico
 :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(partial-completion orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; add to :custom in orderless
;; basic completion style for tramp
;; (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package savehist
  :init (savehist-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; hydra
(use-package hydra
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
      ("e" backward-page)
      ("r" forward-page)
      ("d" beginning-of-defun)
      ("f" end-of-defun)
      ("c" nil "cancel" :color red)))
  :bind ("M-j" . hydra-navigate/body))

;; TODO: consult

;; TODO: abbrew support
;; (global-set-key [select] 'dabbrev-expand)
;; (global-set-key [select] 'hippie-expand)
