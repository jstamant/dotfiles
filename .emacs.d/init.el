;;;;
;;;; S0 - TOC
;;;;
;;;; S1 - GLOBAL SETTINGS
;;;; S2 - MAJOR MODES
;;;; S3 - CUSTOMIZATIONS
;;;;

;;;;
;;;; S1 - GLOBAL SETTINGS
;;;;

;; Set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable the GNU Emacs splash screen
(setq inhibit-splash-screen t)

;; Indentation settings
(setq-default indent-tabs-mode nil) ; do not use tabs for indentation
(setq tab-width 4)

;;;;
;;;; S2 - MAJOR MODES
;;;;

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

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

;;;; PKGBUILD-MODE SETTINGS

;; Load pkgbuild-mode
(add-to-list 'load-path "~/.emacs.d/pkgbuild-mode/")
(load-library "pkgbuild-mode")

(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

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

;; Load web-mode
(add-to-list 'load-path "~/.emacs.d/web-mode/")
(load-library "web-mode")

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

;;;;
;;;; S3 - CUSTOMIZATIONS
;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#504545" "#ad8572" "#a9df90" "#aaca86" "#91a0b3" "#ab85a3" "#ddbc91" "#bdbdb3"])
 '(ansi-term-color-vector
   [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"] t)
 '(cursor-in-non-selected-windows t)
 '(cursor-type (quote box))
 '(custom-enabled-themes nil)
 '(fci-rule-color "#14151E")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#2d2d2d" :foreground "#d3d0c8"))))
 '(cursor ((t (:background "#d3d0c8")))))
