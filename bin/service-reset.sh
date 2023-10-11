#!/bin/sh

service=$(echo -e "dunst - Notification daemon
sxhkd - Hotkey daemon
waybar - Status bar" |
  dmenu -l 10 -p "Reset which service?" | awk '{print $1}' -)

# Kill and start the service once again
pkill $service
$service &

