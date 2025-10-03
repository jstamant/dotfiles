#!/bin/sh

# Set environment variables for login shells; .profile runs for login
# shell, but not for interactive non-login shells
[ -f $HOME/.config/envrc ] && . $HOME/.config/envrc
