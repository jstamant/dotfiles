;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-faces.el
;; Justin R. St-Amant, 2017-02-18
;;
;; Commentary:
;;
;; This file is strongly based off Kaleb Elwert's work in the
;; base16-emacs repository
;; , https://github.com/belak/base16-emacs/
;;
;; The main reason for using the code from the base16-emacs repository
;; is for documentation purposes. I didn't feel like the code in the
;; repository was properly documented, so I rewrote it to my needs and
;; standards.
;;
;; The other reason for using the code was for my own
;; customization. The themes generated by the base16-emacs repository
;; worked, but the settings made org-mode and many other faces too
;; un-coordinated. UNFINISHED
;;
;; UNFINISHED.
;;
;; This file does not actually provide a theme that can be loaded
;; using M-x load-theme. I will need to look more into this and see
;; what I want to do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables / Color sets / Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tinman-base16-colors
  '(:base00 "#2d2d2d"  ; grey18
    :base01 "#393939"  ; grey22
    :base02 "#515151"  ; grey32
    :base03 "#747369"  ; grey45
    :base04 "#a09f93"  ; grey62
    :base05 "#d3d0c8"  ; grey82
    :base06 "#e8e6df"  ; grey92
    :base07 "#f2f0ec"  ; grey94
    :base08 "#f2777a"  ; Pastel red
    :base09 "#f99157"  ; Pastel orange
    :base0A "#ffcc66"  ; Pastel yellow
    :base0B "#99cc99"  ; Pastel green
    :base0C "#66cccc"  ; Pastel cyan
    :base0D "#6699cc"  ; Pastel blue
    :base0E "#cc99cc"  ; Pastel magenta
    :base0F "#d27b53") ; Pastel brown
  "Color palette for Justin St-Amant's Base16-inspired theme.

All colors are currently set to the Base16 Eighties colors.
i.e. soft-grey background with pastel foreground colors.")

(defvar base16-shell-colors
  '(:base00 "black"
    :base01 "brightgreen"
    :base02 "brightyellow"
    :base03 "brightblack"
    :base04 "brightblue"
    :base05 "white"
    :base06 "brightmagenta"
    :base07 "brightwhite"
    :base08 "red"
    :base09 "brightred"
    :base0A "yellow"
    :base0B "green"
    :base0C "cyan"
    :base0D "blue"
    :base0E "magenta"
    :base0F "brightcyan")
  "Base16 colors for use in a terminal.
i.e. with a limited color palette.")

(defvar custom-faces
  '((default
      :foreground base05
      :height 100))
    ;; (link :foreground base0C)
    ;; (link-visited :foreground base0E)
    ;; (font-lock-comment-face  :foreground base03)
    ;; (font-lock-constant-face :foreground base09)
    ;; (font-lock-keyword-face  :foreground base0E)
    ;; (font-lock-string-face   :foreground base0B)
    ;; (outline-1 :foreground base0D)
    ;; (outline-2 :foreground base0A)
    ;; (org-todo :foreground base08)
    ;; (org-done :foreground base0B)
    ;; (org-agenda-structure :foreground base0D)
    ;; (org-special-keyword :foreground base0C)
    ;; (org-link :foreground base0C)
    ;; (highlight :background base03)
    ;; (ledger-font-payee-uncleared-face :foreground base08)
    ;; (ledger-font-other-face :foreground base04)
    ;; (ledger-font-pending-face :foreground base09))
  "A list of faces, and the attributes to apply to them.

The format of this list is as follows:
(face . plist)
(face . plist)
(face . plist)
...

Where face is the name of the face, and plist is a plist of
attributes taking the form:
(attribute value attribute value ... )

Example:
(:foreground \"#00ff00\" :background \"#000000\")

See macro `defface', and look into \"face specs\" or \"face
attributes\" for more info.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme-generation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-theme (theme faces)
  "DOCUMENTATION INCOMPLETE"
  (apply 'custom-theme-set-faces theme (mapcar 'apply-face-spec faces)))

(defun apply-face-spec (face)
  "DOCUMENTATION INCOMPLETE Applies a face spec to a `FACE' for
use with `custom-set-faces' or `custom-theme-set-face'. From a
list containing the face symbol, and a plist of attributes."
  (let ((name       (car face))
        (attributes (cdr face)))
    (list name (list (list '((min-colors 256)) (sub-color-keys attributes))
                     (list t                   (sub-color-keys attributes))))))

(defun sub-color-keys (attributes)
  "DOCUMENTATION INCOMPLETE Substitutes custom color names to
color strings. Also does not work with constants yet."
  (let* ((key   (car attributes))
         (value (cadr attributes))
         (color-key (if (symbolp value) (intern (concat ":" (symbol-name value)) nil)))
         (color (plist-get tinman-base16-colors color-key))
         (output (list key color)))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define an empty custom theme
;;(deftheme tinman-base16
;;  "Justin St-Amant's custom Base16-inspired theme.")

;; Add all the faces to the theme
;;(initialize-theme 'tinman-base16 custom-faces)

;; Mark the theme as provided
;;(provide-theme 'tinman-base16)

(provide 'init-faces)
