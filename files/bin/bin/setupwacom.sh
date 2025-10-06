#!/bin/sh

# Configure Wacom tablet
# See xbindkeyrc for button assignments
devices=$(mktemp)
xsetwacom --list | sed "s/^.*id:/id:/" > $devices
PAD=$(cat $devices | grep "PAD" | awk '{print $2}')
STYLUS=$(cat $devices | grep "STYLUS" | awk '{print $2}')
# (in order of left-to-right)
xsetwacom set $PAD Button 3 11
xsetwacom set $PAD Button 1 12
xsetwacom set $PAD Button 8 13
xsetwacom set $PAD Button 9 14
# Force tablet to 16:9 aspect ratio for computer screens
xsetwacom set $STYLUS Area 0 0 15200 8550
