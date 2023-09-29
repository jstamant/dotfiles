(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-integration t) ; required for evil-collection
  (setq evil-want-keybinding nil) ; required for evil-collection
  :config
  ;;(evil-set-initial-state 'help-mode 'emacs)
  ;;(evil-set-initial-state 'dired-mode 'emacs)
  (advice-add 'evil-search-next :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-search-previous :after
              (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  ;; (advice-add 'evil-scroll-up :after
  ;;             (lambda (&rest x) (move-to-window-line nil)))
  ;; (advice-add 'evil-scroll-down :after
  ;;             (lambda (&rest x) (move-to-window-line nil)))
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection ; Used for better default keybinds in Emacs modes
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Might be cool to implement - keybindings to inc/dec numbers quickly
;; (use-package evil-numbers
;;   :ensure t)

(use-package evil-org ; Provides evil keybinds for org-mode
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'jstamant-evil)
