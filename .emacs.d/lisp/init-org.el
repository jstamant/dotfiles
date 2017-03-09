;;;; ORG-MODE SETTINGS

;; Some general org-mode preferences and global options
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "DEFERRED(f)" "APPT(a)"
                  "|" "DONE(d)" "CANCELLED(c)")))
(put 'org-toggle-time-stamp-overlays 'disabled
     "I don't use timestamp overlays.\n
This command is usually invoked as an accident.\n")
(setq org-refile-targets '((nil . (:level . 1))
                           (nil . (:tag . "project"))))
(setq org-src-fontify-natively t) ; Fontify source blocks
(defun gtd ()
  "Shortcut for finding your GTD file.
This function visits the first file in the `org-files'
variable. It searches your `org-directory'."
  (interactive)
  (find-file (concat org-directory (car org-files))))
(add-hook 'org-mode-hook 'org-indent-mode) ; Clean view by default

;; Set Org files
(defvar org-files
  '("gtd.org")
  "List of your org-mode files for use with Emacs.
When setting this variable in your .emacs, sort them by priority.
i.e. Have your most visited file listed first.")

;; Dynamically set org-directory depending on the OS
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

;; Disable tag inheritance, because I don't make use of it
(setq org-use-tag-inheritance nil)

;; Org-agenda settings
(global-set-key "\C-ca" 'org-agenda)
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-scheduled-leaders '("" ""))
(put 'org-agenda-file-to-front 'disabled
     "Agenda files are determined at startup through the init file!\n")
(put 'org-remove-file 'disabled
     "Agenda files are determined at startup through the init file!\n")
(setq org-stuck-projects
      '("+project/-DONE" ("TODO" "STARTED") nil ""))

(setq org-agenda-custom-commands
      '(("D" "Daily GTD review"
         ((agenda "")
          (todo "WAITING")
          (todo "STARTED")
          (tags-todo "home")
          (tags-todo "anywhere")
          (tags-todo "computer")
          (tags-todo "linux")
          (tags-todo "out")
          (tags-todo "work"))
         ((org-agenda-sorting-strategy
           '((agenda time-up tag-up)
             (tags todo-state-up alpha-up)))))
        ("N" "List of next-actions by context"
         ((tags-todo "home")
          (tags-todo "anywhere")
          (tags-todo "computer")
          (tags-todo "linux")
          (tags-todo "out")
          (tags-todo "work"))
         ((org-agenda-sorting-strategy '((tags todo-state-up alpha-up)))))
        ("W" "WAITING state items" todo "WAITING"
         ((org-agenda-sorting-strategy '((todo tag-up)))))
        ("I" "Incomplete items" todo "STARTED"
         ((org-agenda-sorting-strategy '((todo tag-up)))))
        ("A" "Agenda context" tags-todo "agenda"
         ((org-agenda-sorting-strategy '((todo tag-up)))))
        ("P" "List of active projects" tags "project"
         ((org-agenda-sorting-strategy '((tags alpha-up)))))
        ("T" "Test printable"
         ((todo "TODO|STARTED"))
         ((org-agenda-sorting-strategy '(tag-up timestamp-up))
          (org-agenda-prefix-format "[ ] %-9:T ")
          (org-agenda-todo-keyword-format "%-7s")
          (org-agenda-overriding-header
           "NEXT ACTIONS - sorted by context\n================================\n")
          (org-agenda-remove-tags t)
          (org-agenda-compact-blocks t)
          (ps-number-of-columns 2)
          (ps-landscape-mode t)
          (ps-font-size 20.0)))
        ("t" "Test2"
         ((todo "TODO|STARTED"))
         ((org-agenda-sorting-strategy '(tag-up timestamp-up))
          (org-agenda-prefix-format " %-12:T ")
          (org-agenda-overriding-header
           "NEXT ACTIONS - sorted by context\n================================")
          (org-agenda-remove-tags t)))))

;; org-capture settings
(global-set-key "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "TODO" entry
         (file+headline "gtd.org" "Tasks")
         "* TODO %^{Next-action description}\n%u%?")
        ("c" "Collect" entry
         (file+headline "gtd.org" "In")
         "* %^{Brief description}\n%u%?")))


(provide 'init-org)
