;;;; LEDGER SETTINGS

(use-package ledger-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; Use ISO-8601 date formats, for sh*t's sake
(setq ledger-use-iso-dates t)

(provide 'init-ledger)
