#!/bin/sh

chosen=$(echo -e "displays\nshutdown\nreboot\nhibernate" | dmenu -l 10 -p "Menu:")

case $chosen in
  displays) $HOME/bin/displayselect.sh;;
  shutdown) systemctl poweroff;;
  reboot) systemctl reboot;;
  hibernate) systemctl hibernate ;;
esac
