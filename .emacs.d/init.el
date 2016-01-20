
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar using-windows
  (equal window-system 'w32)
  "t if emacs is running in windows")

;; Load my customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Setup Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable startup messages
(setq inhibit-splash-screen t)

;; Indentation settings
(setq-default indent-tabs-mode nil) ; do not use tabs for indentation
(setq-default tab-width 4)

;; Mode line setting
(line-number-mode t)
(column-number-mode t)

;; Mode settings
(require 'init-auctex)
(require 'init-magit)
(require 'init-markdown)
(require 'init-mode-line)
(require 'init-org)
(require 'init-pkgbuild)
;;(require 'init-scad)
(require 'init-web)
;; While I test lilypond-mode
;;(add-to-list 'load-path (expand-file-name "lilypond-mode" user-emacs-directory))
;;(require 'lilypond-mode)
