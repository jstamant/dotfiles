;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC"))
(rune/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme"))


(provide 'jstamant-keybinds)
