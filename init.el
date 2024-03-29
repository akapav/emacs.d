;; -*- lexical-binding: t; -*-

;; fix
(setq native-comp-deferred-compilation-deny-list nil)

;; setup straight.el
(setq straight-use-package-by-default t
      package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; load the config
(push "~/.emacs.d/config" load-path)
(load "general")
(load "programming")
