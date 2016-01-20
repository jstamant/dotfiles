;;;; ORG-MODE SETTINGS

;; Some Org preferences that I want applied to more than just my
;; common Org files
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|"
                  "DONE(d)" "DEFERRED(f)")))

;; Set Org files
(defvar org-files
  '("tinman.org" "work.org")
  "List of org-mode files for use with Emacs.")

;; Set directory where Org files are located
(when using-windows
  (let ((userprofile
         (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))
         (setq org-directory (concat userprofile "/Google Drive/"))))))
(if (not using-windows)
    (setq org-directory "~/drive/"))

;; Set agenda files
(setq org-agenda-files '())
(dolist (file org-files)
  (if (file-exists-p (concat org-directory file))
      (add-to-list 'org-agenda-files (concat org-directory file))))

;; Org-agenda settings
(global-set-key "\C-ca" 'org-agenda)
(setq org-deadline-warning-days 7)
;;(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(put 'org-agenda-file-to-front 'disabled
     "Agenda files are determined at startup through the init file!\n")
(put 'org-remove-file 'disabled
     "Agenda files are determined at startup through the init file!\n")

(setq org-agenda-custom-commands
      '(("H" "Home-view of all next-actions"
         ((agenda "")
          (tags-todo "home")
          (tags-todo "computer")
          (tags-todo "shopping")
          (tags-todo "spence")
          (tags-todo "phone")
          (tags-todo "work")))
        ("D" "Daily next-actions list"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       '((agenda time-up priority-down tag-up)))
                      (org-deadline-warning-days 0)))))))

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "TODO" entry
         (file+headline "tinman.org" "Tasks")
         "* TODO %^{Brief description} %^g\n%?\nAdded: %U")
        ("c" "Collect" entry
         (file+headline "tinman.org" "In")
         "* %^{Brief description}\n%?\nAdded: %U")))

;; org-mobile settings
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)


(provide 'init-org)
