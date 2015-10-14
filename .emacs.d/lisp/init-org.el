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
;; Agenda files
(setq org-agenda-files '())
(when using-windows
  (if (file-exists-p "c:/Users/jstamant/Google Drive/work.org")
      (add-to-list 'org-agenda-files
                   "c:/Users/jstamant/Google Drive/work.org"))
  (if (file-exists-p "c:/Users/jstamant/Google Drive/tinman.org")
      (add-to-list 'org-agenda-files
                   "c:/Users/jstamant/Google Drive/tinman.org")))
(if (not using-windows)
    (if (file-exists-p "~/drive/tinman.org")
        (add-to-list 'org-agenda-files
                     "~/drive/tinman.org")))

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

(provide 'init-org)
