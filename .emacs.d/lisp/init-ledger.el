;;;; LEDGER SETTINGS

(use-package ledger-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(provide 'init-ledger)
