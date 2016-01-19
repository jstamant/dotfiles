;;;; ORG-MODE SETTINGS

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))

(setq org-deadline-warning-days 7)

;; org-agenda settings
(global-set-key "\C-ca" 'org-agenda)

(put 'org-agenda-file-to-front 'disabled
     "Agenda files are determined at startup through the init file!\n")
(put 'org-remove-file 'disabled
     "Agenda files are determined at startup through the init file!\n")
(setq org-directory "~/drive")

(setq org-agenda-custom-commands
      '(("h" "Home-view of all next actions"
         ((agenda "")
          (tags-todo "home")
          (tags-todo "work")
          (tags-todo "computer")
          (tags-todo "phone")
          (tags-todo "shopping")))
        ("D" "Daily next-actions list"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       '((agenda time-up priority-down tag-up)))
                      (org-deadline-warning-days 0)))))))

;; Agenda files
(setq org-agenda-files '())
(when using-windows
  (let ((userprofile
         (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
  (if (file-exists-p (concat userprofile "/Google Drive/work.org"))
      (add-to-list 'org-agenda-files
                   (concat userprofile "/Google Drive/work.org")))
  (if (file-exists-p (concat userprofile "/Google Drive/tinman.org"))
      (add-to-list 'org-agenda-files
                   (concat userprofile "/Google Drive/tinman.org")))))
(if (not using-windows)
    (if (file-exists-p "~/drive/tinman.org")
        (add-to-list 'org-agenda-files
                     "~/drive/tinman.org")))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "TODO" entry
         (file+headline "C:/Users/jstamant/Google Drive/tinman.org" "Tasks")
         "* TODO %^{Brief description} %^g\n%?\nAdded: %U")))

(provide 'init-org)
