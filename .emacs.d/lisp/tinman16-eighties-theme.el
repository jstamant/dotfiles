;; tinman16-eighties-theme.el
;; 2017-03-28
;; Justin St-Amant
;; This theme is not my design.

(defvar tinman16-eighties-colors
  '(:base00  "#2d2d2d"  ; grey18
    :base01  "#393939"  ; grey22
    :base02  "#515151"  ; grey32
    :base03  "#747369"  ; grey45
    :base04  "#a09f93"  ; grey62
    :base05  "#d3d0c8"  ; grey82
    :base06  "#e8e6df"  ; grey92
    :base07  "#f2f0ec"  ; grey94
    
    :black   "grey18"
    :red     "tomato"   ;"#f2777a" or "light coral"
    :green   "DarkSeaGreen3" ;"#99cc99" or "PaleGreen3" or "LimeGreen" or "LightGreen"
    :yellow  "gold"  ; "#ffcc66" or "gold" or "goldenrod1"
    :blue    "CornflowerBlue" ; "#6699cc" or "SkyBlue3" or "SteelBlue3"
    :magenta "plum3" ; "#cc99cc" or "orchid"
    :cyan    "CadetBlue3" ; "#66cccc" or "MediumTurquoise" or "DarkSlateGray3"
    :white   "gainsboro"
    
    :orange  "salmon1"  ; "#f99157" or "SandyBrown" or "DarkOrange1"
    :brown   "LightSalmon3") ; "#d27b53" or "peru" or "LightSalmon3" or tan3
  "Color palette for Justin St-Amant's Base16-inspired theme.

All colors are currently set to the Base16 Eighties colors.
i.e. soft-grey background with pastel foreground colors.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheme tinman16-eighties)

;; Add all the faces to the theme
(require 'tinman16-theme)
(initialize-theme 'tinman16-eighties tinman16-eighties-colors custom-faces)

(provide-theme 'tinman16-eighties)
