#!/bin/sh

# This file gets executed upon starting the X server from the command-line
[ -f $HOME/.xprofile ] && . $HOME/.xprofile

exec i3
#exec bspwm
#exec dwm
