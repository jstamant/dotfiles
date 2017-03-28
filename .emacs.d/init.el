;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar using-windows
  (equal window-system 'w32)
  "t if emacs is running in windows")

(if using-windows
    (setenv "HOME" (getenv "USERPROFILE")))

(defun init ()
  "Shortcut for finding your Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

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

;; General font settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/")
(load-theme 'tinman16-eighties t)
;; Required; can't be set through the theme. Emacs bug, I think.
(set-face-attribute 'default nil :height 100)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Initialize Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Require use-package, and install it if necessary
(if (not (require 'use-package "use-package" t))
    (progn (package-refresh-contents)
           (package-install 'use-package)))

;; Mail settings (temporary location, to be moved)
(setq message-signature
      "Justin R. St-Amant
Engineering Technologist\n
204-451-7111 (phone)
jstamant24@gmail.com")
(setq message-from-style "angles")
;;(setenv "MAIL" "~/mail")
(setq message-directory "~/mail/")
(setq rmail-preserve-inbox nil) ; Delete mail from the mbox file once retrieved
(setq rmail-primary-inbox-list '("~/mail/inbox"))
(setq rmail-file-name "~/mail/rmail")
(setq rmail-remote-password-required nil)
(setq mail-host-address "gmail.com")
(setq user-full-name "Justin St-Amant")
(setq user-mail-address "jstamant24@gmail.com")
(setq mail-user-agent 'message-user-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; AUCTEX SETTINGS
(use-package tex-site
  :ensure auctex)

;;;; DIRED SETTINGS
(put 'dired-find-alternate-file 'disabled nil)

;;;; LEDGER SETTINGS
(use-package ledger-mode
  :ensure t
  :mode "\\.ledger$"
  :config
  (setq ledger-use-iso-dates t) ; Specify ISO-8601 date format
  (add-to-list 'ledger-reports
               '("uncleared" "%(binary) -f %(ledger-file) reg --uncleared")))

(defun ledger ()
  "Shortcut for finding your ledger file."
  (interactive)
  (find-file "~/drive/reference/budget/2017-test.ledger"))

;;;; MAGIT SETTINGS
(use-package magit
  :ensure t
  :init
  (when using-windows
    (setq magit-git-executable "c:/Program Files/Git/bin/git.exe")))

;;;; MARKDOWN SETTINGS
(use-package markdown-mode
  :ensure t)

;;;; ORG SETTINGS
(require 'init-org)

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t
  :config
  (setq pkgbuild-update-sums-on-save nil))

;;;; SCAD SETTINGS
(use-package scad
  :ensure scad-mode)
(use-package scad-preview
  :ensure t)

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

;; ;;;; WANDERLUST SETTINGS
;; (use-package wanderlust
;;   :ensure t
;;   :mode
;;   ("\\.wl$"      . emacs-lisp-mode)
;;   ("\\.folders$" . conf-unix-mode))

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
