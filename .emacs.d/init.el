;; auto-load pkgbuild-mode and recognize PKGBUILD files
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; place backup and auto-save files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Load additional major-modes
;; I store all my major-modes in ~/.emacs.d/major-mode-name/
(add-to-list 'load-path "~/.emacs.d/web-mode/")
(load-library "web-mode")
