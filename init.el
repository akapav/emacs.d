;; -*- lexical-binding: t; -*-

;; fix
(setq native-comp-deferred-compilation-deny-list nil)

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; list of installed packages
(setq package-selected-packages
      '(avy
        breadcrumb
        browse-kill-ring
        cargo-transient
        chatgpt-shell
        company
        ;;dashboard
        ;;eglot-jl
        eat
        embark
        exec-path-from-shell
        expand-region
        gptel
        hydra
        js2-mode
        ;;julia-mode
        just-mode
        justl
        magit
        marginalia
        orderless
        org-present
        org-roam
        poporg
        popwin
        pyvenv
        rg
        rust-mode
        toml-mode
        typescript-mode
        vertico
        visible-mark
        window-numbering))

;; load the config
(push "~/.emacs.d/config" load-path)
(load "general")
(load "programming")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
