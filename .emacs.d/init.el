;; auto-load pkgbuild-mode and recognize PKGBUILD files
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; place backup and auto-save files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Line number settings
;; Enable line numbering on all buffers by default
;(global-linum-mode t)

;; Indentation settings
(setq-default indent-tabs-mode nil) ; do not use tabs for indentation
(setq tab-width 4)

;; Terminal settings
;term
;term-bold
;term-color-black
;term-color-blue
;term-color-cyan
;term-color-green
;term-color-magenta
;term-color-red
;term-color-white
;term-color-yellow
;term-underline

;; Load additional major-modes
;; I store all my major-modes in ~/.emacs.d/major-mode-name/

;; org-mode settings

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory "~/drive")
(setq org-agenda-files '("~/drive/tinman.org"))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0) ; disable line numbering for org-mode
            )
          )

; add "WAITING" to org's todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE")))

;; web-mode

(add-to-list 'load-path "~/.emacs.d/web-mode/")
(load-library "web-mode")

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            )
          )

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

