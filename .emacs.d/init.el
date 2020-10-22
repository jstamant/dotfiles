;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(defun init ()
  "Shortcut for finding your Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun work ()
  "Shortcut for finding your work org file."
  (interactive)
  (find-file (concat onedrive-directory "work.org")))

;; Enable some pre-disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Do not inherit any colors or settings from X resources
(setq inhibit-x-resources t)
;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load my customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable GUI elements
(menu-bar-mode -1)
(mouse-wheel-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t) ; Disable startup messages

;; Theme settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/") ;; Only required for my own theme
;;(load-theme 'tinman16-eighties t)
;; Required; can't be set through the theme. Emacs bug, I think.
;;(set-face-attribute 'default nil :height 100)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Miscellaneous settings
(setq scroll-preserve-screen-position t) ; Keep point position on screen when scrolling
(setq show-trailing-whitespace t)

;; Initialize Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Require use-package, and install it if necessary
(if (not (require 'use-package "use-package" t))
    (progn (package-refresh-contents)
           (package-install 'use-package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THEME
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-eighties t))

;;;; CALC SETTINGS
(use-package calc
  :bind ("C-x c" . calc))

;;;; DIRED SETTINGS
(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;;;; EVIL SETTINGS
(defvar using-evil nil
  "Set to t   if you are using evil-mode.\nSet to nil if you are not.")
(defun init-evil ()
    "Load and configure packages related to evil-mode."
    (use-package evil-leader
      :ensure t
      :init
      (setq evil-want-C-u-scroll t) ; required before loading evil
      (setq evil-want-keybinding nil)) ; required if evil-collection will be used
    (use-package evil
      :ensure t
      :init
      :config
      (global-evil-leader-mode)
      (evil-set-initial-state 'help-mode 'emacs)
      (evil-set-initial-state 'dired-mode 'emacs)
      (evil-mode 1))
    (use-package evil-collection
      :ensure t
      :after evil
      :config
      (evil-collection-init))
    (use-package evil-org
      :ensure t
      :after org
      :config
      (add-hook 'org-mode-hook 'evil-org-mode)
      (add-hook 'evil-org-mode-hook
                (lambda ()
                  (evil-org-set-key-theme)))
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys)))

(if using-evil init-evil nil)

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

;;;; ORG SETTINGS
(require 'init-org)

;;;; LEDGER SETTINGS
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger$"
  :config
  (setq ledger-default-date-format ledger-iso-date-format) ; YYYY-MM-DD
  (setq ledger-highlight-xact-under-point nil)
  (setq ledger-mode-should-check-version nil)
  (add-to-list 'ledger-reports
               '("uncleared" "%(binary) -f %(ledger-file) reg --uncleared")))
;; See (calc-fix-notation N)

(use-package ledger-report
  :bind (:map ledger-report-mode-map
              ("n"   . next-line)
              ("p"   . previous-line)
              ("TAB" . ledger-report-visit-source)))

(defun ledger ()
  "Shortcut for finding my ledger file."
  (interactive)
  (find-file (concat drive-directory "reference/finances/finances.ledger")))

;;;; MAGIT SETTINGS
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

;;;; MARKDOWN SETTINGS
(use-package markdown-mode
  :ensure t)

;;;; PAREN SETTINGS
(use-package paren
  :bind ("C-x p" . show-paren-mode))

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t
  :config
  (setq pkgbuild-update-sums-on-save nil))

;;;; SHELL-SCRIPT SETTINGS
(use-package sh-script
  :config
  (setq sh-basic-offset 2)
  (add-hook 'sh-mode-hook (lambda () (sh-set-shell "bash"))))

;;;; SMART MODE-LINE SETTINGS
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  ;; Enable line numbering and column numbering
  (setq line-number-mode t)
  (setq column-number-mode t)
  ;; Enable smart mode-line (required)
  (sml/setup))

;;;; TERM SETTINGS
(use-package term)

;;;; AUCTEX SETTINGS
(use-package tex-site
  :ensure auctex)

;;;; VIEW MODE
(use-package view
  :ensure t
  :bind ("C-x v" . view-mode))

;;;; WEB SETTINGS
(use-package web-mode
  :ensure t
  :mode
  "\\.css$"
  "\\.htaccess$"
  "\\.html?$"
  "\\.twig$"
  "\\.php$"
  "\\.xml$"
  :config
  ;; Make .html files recognize Twig templates by default
  (setq web-mode-engines-alist '(("twig" . "\\.html$")))
  ;; web-mode indentation settings
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
