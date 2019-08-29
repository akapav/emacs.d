;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

;; dired
(use-package dired-x
  :init   (setq dired-dwim-target t)
  :config (progn
            (setq dired-omit-files "^\\...+$")
            (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
            (add-hook 'dired-mode-hook 'auto-revert-mode)))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
