;;;; ORG-MODE SETTINGS

;; Ensure org is loaded explicitly, and not lazily
(use-package org
  :ensure t)
(defalias 'o 'org-mode)
;; TODO Ensure org-capture is loaded explicitly, and not lazily

;; Set priorities in org to numeric priorities
(setq org-highest-priority ?1)
(setq org-lowest-priority ?5)
(setq org-default-priority ?5)
;; Set priority faces to be color-coded like in Todoist
;; TODO get this to automatically assign colors from the current theme
(setq org-priority-faces
      '((?1 . "orangered")
        (?2 . "darkorange")
        (?3 . "cornflowerblue")
        (?4 . "darkgray")))

;; Some general org-mode preferences and global options
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"
                  "|" "DONE(d)" "CANCELLED(c)")))
(put 'org-toggle-time-stamp-overlays 'disabled
     "I don't use timestamp overlays.\n
This command is usually invoked as an accident.\n")
(setq org-refile-targets '((nil . (:level . 1))
                           (nil . (:tag . "project"))))
(setq org-src-fontify-natively t) ; Fontify source blocks

;; Aliases for finding your main org file, provided it is an agenda file
(defalias 'gtd  'org-cycle-agenda-files)
(defalias 'work 'org-cycle-agenda-files)
(defalias 'w    'org-cycle-agenda-files)

;; Settings for a clean view
(setq org-adapt-indentation nil) ; Promotes and demotes headings like org used to

;; Set org-directory, agenda files, and main org files
;;Implement some kind of also do a function for 'at home' or 'at work' boolean
;; TODO use org-agenda-file-to-front to bring the default
;; I should also bind org-cycle-agenda-files (C-' and C-,) globally
(if (at-work)
    (progn
      (setq org-directory onedrive-directory)
      (add-to-list 'org-agenda-files (concat org-directory "work.org"))))
(if (at-home)
    (progn
      (setq org-directory drive-directory)
      (add-to-list 'org-agenda-files (concat org-directory "gtd.org"))))
;; DISABLED - set agenda files via C-c [ org-agenda-file-to-front
;; (setq org-agenda-files '())
;; (dolist (file org-files)
;;   (if (file-exists-p (concat org-directory file))
;;       (add-to-list 'org-agenda-files (concat org-directory file))))

;; Disable tag inheritance, because I don't make use of it
(setq org-use-tag-inheritance nil)

;; Org-agenda settings
(global-set-key "\C-ca" 'org-agenda)
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-scheduled-leaders '("" ""))
(setq org-stuck-projects
      '("+project" ("TODO" "STARTED") nil ""))

(setq org-agenda-custom-commands
      '(("d" "Daily report" ; This one is special, it's a composite agenda
         ((tags-todo
           "-TODO\"WAITING\""
           ((org-agenda-overriding-header "Prioritized todo list")))
          (todo
           "WAITING"
           ((org-agenda-overriding-header "Waiting list")))))
        ("u" "Agenda - upcoming" agenda ""
         ((org-agenda-span 30)))
        ("r" "TODO list (r)eport"
         ((tags-todo "-TODO=\"WAITING\""))
         ((org-agenda-sorting-strategy '(priority-down timestamp-up))
          (org-agenda-prefix-format " ")
          (org-agenda-overriding-header "Prioritized TODO list")))
        ("w" "WAITING items" todo "WAITING"
         ((org-agenda-overriding-header "WAITING list")
          (org-agenda-prefix-format " ")
          (org-agenda-sorting-strategy '((tsia-up)))))
        ("i" "Incomplete items" todo "STARTED"
         ((org-agenda-sorting-strategy '((todo tag-up)))))
        ("P" "List of active projects" tags "project"
         ((org-agenda-sorting-strategy '((tags alpha-up)))))))

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(setq org-capture-templates
      `(("t" "TODO" entry
         (file+headline ,(car org-agenda-files) "Tasks")
         "* TODO %^{Next-action description}\n%u%?")
        ("c" "Collect" entry
         (file+headline ,(car org-agenda-files) "In")
         "* %^{Brief description}\n%u%?")
        ("j" "Journal entry" plain
         (file+datetree "reference/journal.org")
         "TEXT\n\nWhat did I accomplish?\n- \n\nWhat did I learn?\n- \n\nWhat am I grateful for?\n- \n\n")))


(provide 'init-org)
