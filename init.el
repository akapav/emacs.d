;; -*- lexical-binding: t; -*-

;; packages setup
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)

;; ensure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; customizations
(setq custom-file "~/.emacs.d/cust.el")
(load custom-file t)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; load config
(push "~/.emacs.d/config" load-path)
(load "general")
;(load "ivycfg")
(load "programming")
(load "apps")
