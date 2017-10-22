#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# Merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Set touchpad preferences
#xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Tapping Enabled" 1
#xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.5
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 1

# Autostart scripts and applications
xbindkeys
# Set desktop wallpaper
~/.fehbg
nm-applet &
udiskie --tray &
insync start

# Execute window manager or desktop environment
#exec cinnamon-session
exec i3

