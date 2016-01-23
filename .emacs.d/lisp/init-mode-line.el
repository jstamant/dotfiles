;; MODE-LINE SETTINGS

(use-package smart-mode-line
  :ensure t)
(setq sml/theme 'dark)
(sml/setup)

;; Enable line numbering and column numbering
(setq line-number-mode t)
(setq column-number-mode t)

(provide 'init-mode-line)
