#!/bin/sh

# Merge in defaults and keymaps

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

if [ -f $userresources ]; then
  xrdb $userresources
fi

if [ -f $usermodmap ]; then
  xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x $f ] && . $f
  done
  unset f
fi

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

# Autostart scripts and applications
xbindkeys
~/.fehbg
nm-applet &
udiskie --tray &
nextcloud &
insync start
numlockx &
compton -C &
dunst &

# Configure statusbar for dwm
#while true; do
#  xsetroot -name "$($HOME/bin/statusbar.sh)"
#  sleep 5s
#done &

# Set DPMS preferences
# Blank screen after 30 minutes
xset s 1800
xset dpms 1800 1800 1800

# Execute window manager or desktop environment
exec i3
