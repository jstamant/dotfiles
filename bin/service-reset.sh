#!/bin/sh

service=$(echo -e "dunst - Notification daemon\nsxhkd - Hotkey daemon" |
  dmenu -l 10 -p "Reset which service?" | awk '{print $1}' -)

# Kill and start the service once again
pkill $service
$service &

#case $service in
#  shutdown) systemctl poweroff;;
#  reboot) systemctl reboot;;
#  hibernate) systemctl hibernate ;;
#esac
