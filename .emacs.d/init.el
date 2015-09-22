;;;;
;;;; S0 - TOC
;;;;
;;;; S1 - GLOBAL SETTINGS
;;;; S2 - MAJOR MODES
;;;;

;;;;
;;;; S1 - GLOBAL SETTINGS
;;;;

;; set backup and auto-save file location
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(setq org-directory "~/drive")
(setq org-agenda-files '("~/drive/tinman.org"))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

;;;; PKGBUILD-MODE SETTINGS

;; auto-load pkgbuild-mode and recognize PKGBUILD files
(add-to-list 'load-path "~/.emacs.d/pkgbuild-mode/")
(load-library "pkgbuild-mode")

(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

;;;; WEB-MODE SETTINGS

;; load web-mode
(add-to-list 'load-path "~/.emacs.d/web-mode/")
(load-library "web-mode")

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            )
          )
