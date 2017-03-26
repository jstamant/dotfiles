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
# Tapping Enabled (274)
xinput set-prop "SynPS/2 Synaptics TouchPad" 274 1
# Accel Speed (282)
#xinput set-prop "SynPS/2 Synaptics TouchPad" 282 0.5
# Natural Scrolling Enabled (284)
xinput set-prop "SynPS/2 Synaptics TouchPad" 284 1

# Autostart scripts and applications
xbindkeys

# Execute window manager or desktop environment
exec startlxde

