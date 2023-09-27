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
  ;; TODO - move these keybindings to somewhere universal?
  (evil-set-leader 'normal " ")
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fv") 'find-alternate-file)
  (evil-define-key 'normal 'global (kbd "<leader>fed") (lambda () (interactive) (find-file user-init-file)))
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'recentf-open-files)
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'list-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>wo") 'other-window)
  (evil-define-key 'normal 'global (kbd "<leader>w1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>w0") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'delete-window)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
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
