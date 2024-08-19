;;; tools/roam/config.el -*- lexical-binding: t; -*-


;; (jrs/emacs-leader-keys
;;   "n" '(:ignore t :which-key "org-roam node")
;;   "nc" 'org-roam-node-capture
;;   "nf" 'org-roam-node-find
;;   "ni" 'org-roam-node-insert
;;   "nl" '(org-roam-buffer-toggle :which-key "links"))

;; TODO figure out the proper defering/autoloading of these
(map!
 :leader
 (:prefix-map ("r" . "roam")
  "c" 'org-roam-node-capture
  "f" 'org-roam-node-find
  "i" 'org-roam-node-insert
  :desc "links" "l" 'org-roam-buffer-toggle))

(after! org-roam
  (org-roam-db-autosync-mode))
