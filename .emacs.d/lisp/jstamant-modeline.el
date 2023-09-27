(require 'use-package)

;; TODO implement doom-modeline (this is what spacemacs uses on graphical displays
;; and implement either sml or vim-powerline for non-graphical displays
(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 15))

(use-package nerd-icons
  :ensure t)
  ;;:config
  ;;(nerd-icons-install-fonts t)) ; TODO only install if missing!

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/theme 'dark)
;;   ;; Enable line numbering and column numbering
;;   (setq line-number-mode t)
;;   (setq column-number-mode t)
;;   ;; Enable smart mode-line (required)
;;   (sml/setup))


(provide 'jstamant-modeline)
