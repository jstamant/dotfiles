;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar using-windows
  (equal window-system 'w32)
  "t if emacs is running in windows")

;; Load my customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable GUI elements
(menu-bar-mode -1)
;;(toggle-scroll-bar -1)
(tool-bar-mode -1)
;; Disable startup messages
(setq inhibit-splash-screen t)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*"   ,temporary-file-directory t)))

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Setup Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu"   . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; AUCTEX SETTINGS
(use-package tex-site
  :ensure auctex)

;;;; EVIL SETTINGS
;;(require 'init-evil)

;;;; LEDGER SETTINGS
(use-package ledger-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
;; Use ISO-8601 date formats, for sh*t's sake
(setq ledger-use-iso-dates t)

;;;; MAGIT SETTINGS
(when using-windows
  (setq magit-git-executable "c:/Program Files/Git/bin/git.exe"))
(use-package magit
  :ensure t)

;;;; MARKDOWN SETTINGS
(use-package markdown-mode
  :ensure t)

;;;; MODE-LINE SETTINGS
(use-package smart-mode-line
  :ensure t)
(setq sml/theme 'dark)
(sml/setup)
;; Enable line numbering and column numbering
(setq line-number-mode t)
(setq column-number-mode t)

;;;; ORG SETTINGS
(require 'init-org)

;;;; PKGBUILD SETTINGS
(use-package pkgbuild-mode
  :ensure t)
(setq pkgbuild-update-sums-on-save nil)

;;;; SCAD SETTINGS
(use-package scad
  :ensure scad-mode)
(use-package scad-preview
  :ensure t)

;;;; WEB-MODE SETTINGS
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
;; No reason to use plain HTML unless it's for a template engine!
(setq web-mode-engines-alist '(("twig" . "\\.html\\'")))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))
