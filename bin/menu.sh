#!/bin/sh

chosen=$(echo -e "ledger\ndisplays\nshutdown\nreboot\nhibernate" | dmenu -l 10 -p "Menu:")

case $chosen in
  ledger) emacs -f ledger;;
  displays) $HOME/bin/displayselect.sh;;
  shutdown) systemctl poweroff;;
  reboot) systemctl reboot;;
  hibernate) systemctl hibernate ;;
esac
