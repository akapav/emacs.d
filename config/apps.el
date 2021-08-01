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
            (setq dired-dwim-target t))
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

;; mu4e
(setq user-mail-address "aka@gensym.net"
      user-full-name "alan pavičić")

(use-package mu4e
  :straight nil
  :init
  (progn
    (setq
     mue4e-headers-skip-duplicates  t
     mu4e-view-show-images t
     mu4e-view-show-addresses t
     mu4e-compose-format-flowed nil
     mu4e-date-format "%y/%m/%d"
     mu4e-headers-date-format "%Y/%m/%d"
     mu4e-change-filenames-when-moving t
     mu4e-attachments-dir "~/dl"

     mu4e-maildir       "~/mail/fastmail"   ;; top-level Maildir
     ;; note that these folders below must start with /
     ;; the paths are relative to maildir root
     mu4e-refile-folder "/Archive"
     mu4e-sent-folder   "/Sent"
     mu4e-drafts-folder "/Drafts"
     mu4e-trash-folder  "/Trash")

    ;; this setting allows to re-sync and re-index mail
    ;; by pressing U
    (setq mu4e-get-mail-command  "mbsync -a")


    (setq
     smtpmail-debug-info t
     message-send-mail-function 'smtpmail-send-it
     smtpmail-default-smtp-server "smtp.fastmail.com"
     smtpmail-smtp-server         "smtp.fastmail.com"
     smtpmail-stream-type 'ssl
     smtpmail-smtp-service 465)))
