#!/bin/sh

DMENULAUNCHER=dmenu
if [[ $WAYLAND_DISPLAY ]]; then
    DMENULAUNCHER="rofi -dmenu"
fi

chosen=$(echo -e "xorg-xeyes\nprinters\nscanners\ndisplays\nshutdown\nreboot\nhibernate" | $DMENULAUNCHER -l 10 -p "Menu:")

case $chosen in
  #ledger) emacs -f ledger;;
  xorg-xeyes) xeyes;;
  printers) system-config-printer;;
  scanners) simple-scan;;
  displays) $HOME/bin/displayselect.sh;;
  shutdown) systemctl poweroff;;
  reboot) systemctl reboot;;
  hibernate) systemctl hibernate ;;
esac
