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

;; Theme settings
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

;;;;
;;;; S2 - MAJOR MODES
;;;;

;;;; ORG-MODE SETTINGS

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))

;; org-agenda settings
(global-set-key "\C-ca" 'org-agenda)
(setq org-directory "~/drive")
(setq org-agenda-files '("~/drive/tinman.org"))

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
 '(custom-safe-themes
   (quote
    ("e7ebd15131a75a551e84cf7168e2d01d576f4905c3008d230ca5a8e981405a44" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "8b30636c9a903a9fa38c7dcf779da0724a37959967b6e4c714fdc3b3fe0b8653" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "72ac74b21322d3b51235f3b709c43c0721012e493ea844a358c7cd4d57857f1f" "38d25871e95642ee1a13013bdb988e8c8fcb4ced3832d3e927c7296a0cdf5f59" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default)))
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
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#d3d0c8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 112 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
