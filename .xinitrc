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

# Autostart scripts and applications
xbindkeys
# Set desktop wallpaper
~/.fehbg
nm-applet &
udiskie --tray &
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
