#!/bin/sh

# A simple UI for selecting a terminal color scheme from a list of .Xresources

# List only base16 themes, and get rid of all 256-style themes
theme_list=$(mktemp)
for file in ~/.Xresources.d/base16-themes/*; do
  if [[ -f $file ]]; then
    echo $(basename $file) >> $theme_list
  fi
done
themes=$(mktemp)
cat $theme_list | grep "base16" | grep -v "256" | \
  sed 's/base16-\(.*\)\..*/\1/' > $themes
chosen=$(cat $themes | dmenu -i)
if [ $chosen ]; then
  ln -fs "$HOME/.Xresources.d/base16-themes/base16-$chosen.Xresources" \
    "$HOME/.Xresources.d/theme"
  # This .Xresources file should include ~/.Xresources.d/base16-theme
  xrdb $HOME/.Xresources
fi
