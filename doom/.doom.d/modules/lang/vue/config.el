;;; lang/vue/config.el -*- lexical-binding: t; -*-

;; TODO do I need to move this autoload?
(autoload 'web-mode "web-mode")
(define-derived-mode vue-mode web-mode "Vue")

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;;(add-hook 'vue-mode-hook 'lsp-deferred)

;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-mode-alist '(vue-mode . prettier-javascript)))
