;; tinman16-eighties-theme.el
;; 2017-03-28
;; Justin St-Amant
;; This theme is not my design.

(defvar tinman16-eighties-colors
  '(:base00 "#2d2d2d"
    :base01 "#393939"
    :base02 "#515151"
    :base03 "#747369"
    :base04 "#a09f93"
    :base05 "#d3d0c8"
    :base06 "#e8e6df"
    :base07 "#f2f0ec"
    :base08 "#f2777a"
    :base09 "#f99157"
    :base0A "#ffcc66"
    :base0B "#99cc99"
    :base0C "#66cccc"
    :base0D "#6699cc"
    :base0E "#cc99cc"
    :base0F "#d27b53")
  "Color palette for Justin St-Amant's Base16-inspired theme.

All colors are currently set to the Base16 Eighties colors.
i.e. soft-grey background with pastel foreground colors.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheme tinman16-eighties)

;; Add all the faces to the theme
(require 'tinman16-theme)
(initialize-theme 'tinman16-eighties tinman16-eighties-colors custom-faces)

(provide-theme 'tinman16-eighties)
