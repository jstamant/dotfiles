;; tinman16-3024-theme.el
;; 2017-03-26
;; Justin R. St-Amant

(defvar tinman16-3024-colors
  '(:base00  "#090300"
    :base01  "#3a3432"
    :base02  "#4a4543"
    :base03  "#5c5855"
    :base04  "#807d7c"
    :base05  "#a5a2a2"
    :base06  "#d6d5d4"
    :base07  "#f7f7f7"
    :base08  "#db2d20"
    :base09  "#e8bbd0"
    :base0A  "#fded02"
    :base0B  "#01a252"
    :base0C  "#b5e4f4"
    :base0D  "#01a0e4"
    :base0E  "#a16a94"
    :base0F  "#cdab53"

    :black   "#090300"
    :red     "#db2d20"
    :green   "#01a252"
    :yellow  "#fded02"
    :blue    "#01a0e4"
    :magenta "#a16a94"
    :cyan    "#b5e4f4"
    :white   "#a5a2a2"

    :orange  "#e8bbd0"
    :brown   "#cdab53")
  "All colors for Base16 3024 are defined here.")

;; Define the theme
(deftheme tinman16-3024)

;; Add all the faces to the theme
(require 'tinman16-theme)
(initialize-theme 'tinman16-3024 tinman16-3024-colors custom-faces)

;; Mark the theme as provided
(provide-theme 'tinman16-3024)

