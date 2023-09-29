;; General provides a clean and convenient way of managing keybinds
;; and prefixes across packages

(use-package general
  :ensure t)

;; TO MOVE THESE INTO USE-PACKAGE??
;;(general-evil-setup) ; Use this if you want key definers like general-nmap or general-imap

(general-create-definer jstamant/leader-key
  :prefix "SPC"
  :global-prefix "M-SPC")
;;:non-normal-prefix "M-SPC" ; What about this? Using non-normal-prefix

(general-create-definer jstamant/local-leader-key
  :prefix ","
  :global-prefix "M-,")

;; TODO REQUIRE HELPFUL?
(jstamant/leader-key
  :keymaps 'normal
  "" '(nil :which-key "<leader>")
  "b" '(:ignore t :which-key "buffers")
  "f" '(:ignore t :which-key "files")
  "g" '(:ignore t :which-key "git")
  "gg" 'magit
  "gs" 'magit-status
  "h" '(:ignore t :which-key "help")
  "hd" '(:ignore t :which-key "describe")
  "hdf" 'helpful-callable
  ;;"hdf" 'helpful-callable ;TODO add a describe-face?
  "hdm" 'describe-mode
  "hdv" 'helpful-variable
  "t"  '(:ignore t :which-key "toggles") ;not added by me
  "tt" '(counsel-load-theme :which-key "choose theme") ;not added by me
  "w" '(:ignore t :which-key "windows")
  "fs" 'save-buffer
  "ff" 'find-file
  "fv" 'find-alternate-file
  "fe" '(:ignore t :which-key "emacs-related files")
  "fed" '((lambda () (interactive) (find-file user-init-file)) :which-key "dotfile")
  "fee" '((lambda () (interactive) (find-file (expand-file-name "jstamant-evil.el" user-lisp-directory))) :which-key "evil")
  "fek" '((lambda () (interactive) (find-file (expand-file-name "jstamant-keybinds.el" user-lisp-directory))) :which-key "keybinds")
  "fr" 'recentf-open-files
  "fd" 'dired
  "bb" 'switch-to-buffer
  "bl" 'list-buffers
  "bd" 'kill-buffer
  "bk" 'kill-buffer
  "ww" 'other-window
  "wo" 'other-window
  "w1" 'delete-other-windows
  "w0" 'delete-window
  "wd" 'delete-window
  "wk" 'delete-window)

(jstamant/local-leader-key
  :keymaps 'normal
  "" '(:ignore t :which-key "mode-specific bindings")
  "h" 'describe-mode)

(general-define-key 
 :keymaps '(normal insert)
 "C-a" 'move-beginning-of-line
 "C-e" 'move-end-of-line)


(provide 'jstamant-keybinds)
