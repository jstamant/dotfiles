
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
(setq tab-width 4)

;; Mode line settings
(use-package smart-mode-line
  :ensure t)
(setq sml/theme 'dark)
(sml/setup)

;; General auto-mode settings
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-mode))

;; Major mode settings
(use-package markdown-mode
  :ensure t)
(require 'init-org)
(use-package pkgbuild-mode
  :ensure t)
(use-package scad
  :ensure scad-mode)
(use-package scad-preview
  :ensure t)
;;;; TERM-MODE SETTINGS
;; Set color scheme
;;(custom-set-variables
;; )
;;(custom-set-faces
;; '(term-color-blue ((t (:background "red" :foreground "red")))))
;;term              
;;term-bold         
;;term-color-black  
;;term-color-blue   
;;term-color-cyan   
;;term-color-green  
;;term-color-magenta
;;term-color-red    
;;term-color-white  
;;term-color-yellow 
;;term-underline
(require 'init-web)
