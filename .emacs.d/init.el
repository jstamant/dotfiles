;; FOR TESTING
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)



(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load my customizations
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(if (not (file-exists-p custom-file)) (with-temp-buffer (write-file custom-file)))
(load custom-file 'noerror 'nomessage)

;; Initialize Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Require use-package, and install it if necessary
;; TODO - remove this - no need to install, it's included withe Emacs 29.1
(if (not (require 'use-package "use-package" t))
    (progn (package-refresh-contents)
           (package-install 'use-package)))
;;(setq use-package-always-ensure t)
;; TODO - don't refresh packages if you don't have to! What about this:
;; (unless package-archive-contents
;;   (package-refresh-contents))

(setq user-full-name "Justin St-Amant")

(defvar using-windows
  (equal window-system 'w32)
  "t if emacs is running in windows")

(defvar drive-directory
  (if (not using-windows)
      "~/drive/"
    (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
      (concat userprofile "/Google Drive/")))
  "The absolute path to the Google Drive directory under Linux or Windows.")

(defvar onedrive-directory
  (if using-windows
      (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
        (concat userprofile "/OneDrive - Manitoba Hydro/")))
  "The absolute path to your work OneDrive directory. Only for Windows.")

(if using-windows
    (setenv "HOME" (getenv "USERPROFILE")))

(defun at-work ()
  "t if using a work computer."
  (equal "MH" (substring (system-name) 0 2)))
(defun at-home ()
  "t if using a non-work computer."
  (not (at-work)))

(defun init ()
  "Shortcut for finding your Emacs configuration file.
Finds `user-init-file'"
  (interactive)
  (find-file user-init-file))
(defun reload ()
  "Shortcut for reloading your Emacs configuration.
This is great for when you're tinkering on your `user-init-file'"
  (interactive)
  (load user-init-file))

;; Enable some pre-disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Do not inherit any colors or settings from X resources
(setq inhibit-x-resources t)
;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Disable GUI elements
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 10)        ; Give some breathing room??
(setq inhibit-startup-screen t) ; Disable the default Emacs startup screen
(setq use-dialog-box nil) ; Disables dialog boxes for mouse-driven actions

;; Sound settings
(setq ring-bell-function 'ignore) ; Turn off audible bell
(setq visible-bell t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Miscellaneous settings
(setq scroll-preserve-screen-position t) ; Keep point position on screen when scrolling
(setq show-trailing-whitespace t)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;(set-face-attribute 'default nil :font "Deja Vu Sans Mono" :height 100) ; ??????

(column-number-mode 1)
;; TODO - add a toggle for this
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Or...Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode
  :ensure t)

;;;; THEMES
(use-package base16-theme
  :ensure t)
(use-package spacemacs-theme
  :ensure t)
(global-hl-line-mode 1)
(use-package modus-themes
  :ensure t)
(load-theme 'spacemacs-dark t)
;; Configure the Modus Themes' appearance
;;(setq modus-themes-mode-line '(accented borderless)
;;      modus-themes-bold-constructs t
;;      modus-themes-italic-constructs t
;;      modus-themes-fringes 'subtle
;;      modus-themes-tabs-accented t
;;      modus-themes-paren-match '(bold intense)
;;      modus-themes-prompts '(bold intense)
;;      modus-themes-completions 'opinionated
;;      modus-themes-org-blocks 'tinted-background
;;      modus-themes-scale-headings t
;;      modus-themes-region '(bg-only)
;;      modus-themes-headings
;;      '((1 . (rainbow overline background 1.4))
;;        (2 . (rainbow background 1.3))
;;        (3 . (rainbow bold 1.2))
;;        (t . (semilight 1.1))))
;; Load the dark theme by default
;;(load-theme 'modus-vivendi t)
;;(blink-cursor-mode -1)

;; Mode-line settings
(use-package jstamant-modeline)

(setq delete-by-moving-to-trash t) ; Use trash instead of destroying files

(use-package savehist
  :config
  (setq history-length 100)
  (savehist-mode 1))
(use-package saveplace
  :config
  (save-place-mode 1))

;; Set sentences to be identified by a period and a single space, instead of two spaces
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Make a quick function to insert today's date in ISO 8601
(defun dts ()
  "Insert today's date in ISO 8601 (YYYY-MM-DD)."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ALIASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Major-mode aliases
(defalias 'o 'org-mode)
(defalias 'org 'org-mode)

;; Minor-mode aliases
(defalias 'afm 'auto-fill-mode)
(defalias 'fsm 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; AUTO-REVERT MODE SETTINGS
;;TODO NEED TO ADD A TOGGLE FOR AUTO-REVERT-MODE
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;;;; CALC SETTINGS
(use-package calc
  :bind ("C-x c" . calc))

;;;; C/C++ MODE SETTINGS
(use-package cc-mode
  :ensure t
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4))

;;;; DIRED SETTINGS
(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("C-s" . dired-isearch-filenames)
              ("TAB" . dired-find-file)))
;; Place this Inside `use-package dired`???
;; (use-package dired-single)
;;     (evil-collection-define-key 'normal 'dired-mode-map
;;       "h" 'dired-single-up-directory
;;       "l" 'dired-single-buffer)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; (use-package dired-open
;;   :config
;;   ;; Doesn't work as expected!
;;   (add-to-list 'dired-open-functions #'dired-open-xdg t)
;;   ;; -- OR! --
;;   (setq dired-open-extensions '(("png" . "feh")
;;                                 ("mkv" . "mpv"))))
;; (use-package dired-hide-dotfiles
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "H" 'dired-hide-dotfiles-mode))

;;;; EMOJIFY SETTINGS
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

;;;; EVIL SETTINGS
(use-package jstamant-evil)

;;;; FLYSPELL SETTINGS
(use-package flyspell
  :hook (text-mode . flyspell-mode)) ; Start spell checking on all modes derived from text-mode

;;;; HELM-MODE SETTINGS
;;(use-package helm
;;  :ensure t)

;;;; HELP-MODE SETTINGS
(use-package help-mode
  :bind (:map help-mode-map
              ;; Some additional navigation bindings
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . forward-char)
              ("b" . backward-char)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("v" . scroll-up-command)
              ("V" . scroll-down-command)
              ("C-d" . scroll-up-command)))

;;;; ISPELL SETTINGS
(use-package ispell
  :config
  (setq ispell-program-name "/usr/bin/aspell")) ; Change default spell checking program from ispell to aspell

;;;; IVY SETTINGS
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;;;; LEDGER SETTINGS
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :config
  (setq ledger-default-date-format ledger-iso-date-format) ; YYYY-MM-DD
  (setq ledger-highlight-xact-under-point nil) ; Screen is less cluttered without xact highlighting
  (setq ledger-mode-should-check-version nil) ; Ignore checking the 'ledger' binary
  (setq ledger-clear-whole-transactions t) ; For reconciliation, clear whole transactions, doesn't work great. It would be nice to have this only set during reconcile-mode
  (add-to-list 'ledger-reports
               '("uncleared" "%(binary) -f %(ledger-file) reg --uncleared"))
  ;; Fix auto-completion for ledger accounts
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t))))

(use-package ledger-report
  :bind (:map ledger-report-mode-map
              ("n"   . next-line)
              ("p"   . previous-line)
              ("TAB" . ledger-report-visit-source)))

(defun ledger ()
  "Shortcut for finding my ledger file, and navigating to the end
of the buffer."
  (interactive)
  (find-file (concat drive-directory "reference/finances/finances.ledger"))
  (end-of-buffer))

;;;; LSP SETTINGS
(use-package jstamant-lsp)

;;;; MAGIT SETTINGS
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

;; https://magit.vc/manual/forge/
;; https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :ensure t)

;;;; MARKDOWN SETTINGS
(use-package markdown-mode
  :ensure t)

;;;; MULTIPLE-CURSORS SETTINGS
(use-package multiple-cursors
  :ensure t
  :bind ("C-c m c" . mc/edit-lines))

;;;; ORG-MODE SETTINGS
(use-package jstamant-org)

;;;; PAREN SETTINGS
(use-package paren
  :bind ("C-x p" . show-paren-mode))

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t
  :config
  (setq pkgbuild-update-sums-on-save nil))

;;;; RAINBOW DELIMITERS SETTINGS
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; RECENTF SETTINGS
(use-package recentf
  :config (recentf-mode 1))

;;;; SHELL-SCRIPT SETTINGS
(use-package sh-script
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))

;;;; TERM SETTINGS
(use-package term)

;;;; TREESITTER SETTINGS
(use-package jstamant-treesitter)

;;;; UNFILL SETTINGS
(use-package unfill
  :ensure t)

;;;; VIEW MODE
(use-package view
  :ensure t
  :bind ("C-x v" . view-mode))

;;;; WEB SETTINGS
(use-package web-mode
  :ensure t
  :mode
  "\\.css\\'"
  "\\.htaccess\\'"
  "\\.html?\\'"
  "\\.twig\\'"
  "\\.php\\'"
  "\\.xml\\'"
  :config
  ;; Make .html files recognize Twig templates by default
  (setq web-mode-engines-alist '(("twig" . "\\.html\\'")))
  ;; web-mode indentation settings
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;;;; WHICH-KEY SETTINGS
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2.0))




(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))


;; Helpful provides us with a help-mode that shows prettier and better organized
;; help content than the default help-mode
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; Hydra is a package that lets you call functions at the end of a prefix
;; group without needing to call the prefix again.
;; The best example is the zoom in/out Hydra
(use-package hydra
  :ensure t)
;; (defhydra hydra-text-scale (:timeout 4)
;;           "scale text"
;;           ("j" text-scale-increase "in")
;;           ("k" text-scale-decrease "out")
;;           ("f" nil "finished" :exit t))
;; (defhydra hydra-zoom (global-map "<f2>")
;;           "zoom"
;;           ("g" text-scale-increase "in")
;;           ("l" text-scale-decrease "out"))
;; (rune/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/programming")
    (setq projectile-project-search-path '("~/programming")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode 1))

(use-package jstamant-keybinds)

;; (use-package company
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package lsp-treemacs
  :ensure t
  :after lsp)

(use-package lsp-ivy
  :ensure t)

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))
;; ;;npm install -g typescript-language-server

(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
