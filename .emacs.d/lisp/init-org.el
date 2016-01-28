;;;; ORG-MODE SETTINGS

;; Some general org-mode preferences and global options
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "DEFERRED(f)" "APPT(a)"
                  "|" "DONE(d)" "CANCELLED(c)")))
(put 'org-toggle-time-stamp-overlays 'disabled
     (concat "I don't use timestamp overlays.\n"
             "This command is usually invoked as an accident.\n"))

;; Set Org files
(defvar org-files
  '("gtd.org" "work.org")
  "List of org-mode files for use with Emacs.")

;; Dynamically set directory where Org files are located
(when using-windows
  (let ((userprofile
         (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
    (setq org-directory (concat userprofile "/Google Drive/"))))
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

;; Disable tag inheritance, because I don't make use of it
(setq org-use-tag-inheritance nil)

(setq org-agenda-custom-commands
      '(("D" "Daily GTD review"
         ((agenda "")
          (todo "WAITING")
          (tags-todo "home")
          (tags-todo "computer")
          (tags-todo "spence")
          (tags-todo "shopping")
          (tags-todo "work"))
         ((org-agenda-sorting-strategy
           '((agenda time-up tag-up)
             (tags todo-state-up alpha-up)))))
        ("N" "List of next-actions by context"
         ((tags-todo "home")
          (tags-todo "computer")
          (tags-todo "spence")
          (tags-todo "shopping")
          (tags-todo "work"))
         ((org-agenda-sorting-strategy
           '((tags todo-state-up alpha-up)))))
        ("W" "WAITING state items" todo "WAITING")
        ("P" "List of active projects" tags "project")))

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "TODO" entry
         (file+headline "gtd.org" "Tasks")
         "* TODO %^{Next-action description} %^g\n%?\n")
        ("c" "Collect" entry
         (file+headline "gtd.org" "In")
         "* %^{Brief description}\n%?\n")))

;; org-mobile settings
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/drive/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)


(provide 'init-org)
