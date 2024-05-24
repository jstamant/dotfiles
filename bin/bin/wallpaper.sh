#!/bin/sh

# Simple wallpaper-selector script
# Also updates the wallpaper in the current X session

wallpaper_directory=$HOME/images/wallpaper
wallpaper_link=$wallpaper_directory/wall

if [[ ! -d $wallpaper_directory ]]; then
  echo "error: wallpaper directory does not exist!"
  echo "debug the script at $(realpath $0)"
  echo "exiting..."
  exit
fi

# Select wallpaper using dmenu <3
wallpaper_list=$(mktemp)
for image in $(ls -1 $wallpaper_directory | grep "jpg"); do
  echo $image >> $wallpaper_list
done
chosen=$(cat $wallpaper_list | dmenu -i -l 10 -p "Select a wallpaper:")

# Change wallpaper and update it in the current session
if [[ -e $wallpaper_link ]]; then
  rm $wallpaper_link
fi
ln -s $wallpaper_directory/$chosen $wallpaper_link
$HOME/.fehbg
