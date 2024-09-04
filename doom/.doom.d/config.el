;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Justin St-Amant")
;; (setq user-mail-address "jstamant24@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(defvar drive-directory
  (cond ((equal system-type 'gnu/linux)
         "~/drive/")
        ((equal system-type 'darwin)
         (cond ((file-directory-p "~/Google Drive/My Drive/")
                ;; This is when only one account is added
                "~/Google Drive/My Drive/")
               ((file-directory-p "~/jstamant24@gmail.com - Google Drive/My Drive/")
                ;; This is when more than one accounts are added
                "~/jstamant24@gmail.com - Google Drive/My Drive/")))
        ((equal system-type 'windows-nt)
         (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
           (concat userprofile "/Google Drive/"))))
  "The absolute path to the Google Drive directory under any operating system.")

(defvar onedrive-directory
  (when (eq system-type 'windows-nt)
    (let ((userprofile (replace-regexp-in-string "\\\\" "/" (getenv "USERPROFILE"))))
      (concat userprofile "/OneDrive - Manitoba Hydro/")))
  "The absolute path to your work OneDrive directory. Only for Windows.")

(when (eq system-type 'windows-nt)
  (setenv "HOME" (getenv "USERPROFILE")))

(defun at-work ()
  "t if using a work computer."
  (equal "MH" (substring (system-name) 0 2)))
(defun at-home ()
  "t if using a non-work computer."
  (not (at-work)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Start emacs maximized, WM doesn't seem to control the frame size initially
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(map! :leader :prefix "o" "c" 'calc)

(map! "M-o" 'other-window)
(map! :leader "k" 'kill-this-buffer)

(map! :leader "fj" 'dired-jump)

(map!
 :leader
 "0" 'delete-window
 "1" 'delete-other-windows)

(setq transient-levels-file (concat doom-user-dir ".local/etc/transient/levels"))

(setq org-startup-indented nil)

;; Remove the annoying suggestions for command abbreviations
(setq extended-command-suggest-shorter nil)

;; TODO will these take affect?
(setq evil-want-C-u-delete nil) ;; nope

;; TODO add a toggle for this?
(global-page-break-lines-mode)

;; TODO make link face not bold if using this...
;; (global-goto-address-mode)

(auto-revert-mode)

(setq-default abbrev-mode t)

;; Use trash instead of destroying files
(setq delete-by-moving-to-trash t)

(setq pkgbuild-update-sums-on-save nil)

(map! :prefix "C-x" "v" 'view-mode)

(defun headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert "\n;;; " fname " ends here\n"))))

(let ((directory (cond ((eq system-type 'gnu/linux)
                        "~/programming")
                       ((eq system-type 'darwin)
                        "~/Developer"))))
  (when (file-directory-p directory)
    (setq projectile-project-search-path `(,directory))))
;; (setq projectile-switch-project-action #'projectile-find-file)

;; org-mode config
(setq org-log-done 'time) ; Log closing-time of tasks
;; Set org-directory, agenda files, and main org files
;; Implement some kind of also do a function for 'at home' or 'at work' boolean
;; I should also bind org-cycle-agenda-files (C-' and C-,) globally
(after! org
  (if (at-work)
      (progn
        (setq org-directory onedrive-directory)
        (add-to-list 'org-agenda-files (concat org-directory "work.org"))))
  (if (at-home)
      (progn
        (setq org-directory drive-directory)
        (add-to-list 'org-agenda-files (concat org-directory "gtd.org")))))
