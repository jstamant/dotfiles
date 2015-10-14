;;;; MAGIT SETTINGS

(when using-windows
  (setq magit-git-executable "c:/Program Files/Git/bin/git.exe"))

(use-package magit
  :ensure t)

(provide 'init-magit)
