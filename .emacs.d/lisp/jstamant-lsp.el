;; Main documentation page for lsp-mode is located at:
;; https://emacs-lsp.github.io/lsp-mode/
;; https://emacs-lsp.github.io/lsp-mode/page/languages/

(use-package lsp-mode
  :ensure t
  :after which-key
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))
;;(XXX-mode . lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))


(provide 'jstamant-lsp)
