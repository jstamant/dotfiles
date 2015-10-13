;;;;
;;;; S0 - TOC
;;;;
;;;; S1 - GLOBAL SETTINGS
;;;; S2 - MAJOR MODES
;;;;

;;;;
;;;; S1 - GLOBAL SETTINGS
;;;;

;; Load my customizations
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Setup Emacs package manager
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Add managed packages to the load-path
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
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
(require 'smart-mode-line)
(setq sml/theme 'dark)
(sml/setup)

;; General auto-mode settings
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-mode))

;;;;
;;;; S2 - MAJOR MODES
;;;;

;;;; MARKDOWN-MODE SETTINGS

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; ORG-MODE SETTINGS

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))

;; org-agenda settings
(global-set-key "\C-ca" 'org-agenda)
(put 'org-agenda-file-to-front 'disabled
     "Agenda files are determined at startup through the init file!\n")
(put 'org-remove-file 'disabled
     "Agenda files are determined at startup through the init file!\n")
(setq org-directory "~/drive")
;; Set org-agenda-files according to my location
(setq org-agenda-files '())
;; Agenda files at home
(if (file-exists-p "~/drive/tinman.org")
    (add-to-list 'org-agenda-files
                 "~/drive/tinman.org"))
;; Agenda files at work
(if (file-exists-p "c:/Users/jstamant/Google Drive/work.org")
    (add-to-list 'org-agenda-files
                 "c:/Users/jstamant/Google Drive/work.org"))
(if (file-exists-p "c:/Users/jstamant/Google Drive/tinman.org")
    (add-to-list 'org-agenda-files
                 "c:/Users/jstamant/Google Drive/tinman.org"))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

;;;; PKGBUILD-MODE SETTINGS

(require 'pkgbuild-mode)

(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;;;; SCAD-MODE SETTINGS

(require 'scad "scad-mode")
(require 'scad-preview)

;;(add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode))

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

;;;; WEB-MODE SETTINGS

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
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

