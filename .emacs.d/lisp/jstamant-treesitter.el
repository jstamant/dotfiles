(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :after tree-sitter)


(provide 'jstamant-treesitter)
