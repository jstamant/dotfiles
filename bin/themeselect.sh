#!/bin/sh

# A simple UI for selecting a terminal color scheme from a list of .Xresources
# Pretty shabby script, I must say, but hey :/

themefile=$HOME/.Xresources.d/theme

# List only dark base16 themes, and get rid of all 256-style themes
theme_list=$(mktemp)
for file in ~/.Xresources.d/base16-themes/*; do
  if [ -f "$file" ]; then
    basename "$file" >> "$theme_list"
  fi
done
themes=$(mktemp)
grep "base16" "$theme_list" | grep -v "light" | grep -v "256" | \
  sed 's/base16-\(.*\)\..*/\1/' > "$themes"

# Prompt the user to select a theme
if [ -f "$themefile" ]; then
  themefilepath=$(realpath "$themefile")
  themefilename=$(basename -s .Xresources "$themefilepath" | sed 's/base16-//')
  current_theme=$themefilename
  # Add the current theme as default selection
  selection=$(mktemp)
  echo "$themefilename" > "$selection"
  grep -v "$themefilename" "$themes" >> "$selection"
else
  current_theme="none"
fi
chosen=$(dmenu -i -p "Select theme: [$current_theme]" < "$selection")
if [ "$chosen" ]; then
  ln -fs "$HOME/.Xresources.d/base16-themes/base16-$chosen.Xresources" "$themefile"
  # This .Xresources file should include ~/.Xresources.d/base16-theme
  xrdb "$HOME"/.Xresources
fi
