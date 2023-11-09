#!/bin/bash

# ==================================================
# ~/.bashrc
# ==================================================

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Standard Arch Linux PS1 in blue
# [jstamant@t460 ~]$ ls -la
export PS1='\[\e[1;34m\][\u@\h \W]\$\[\e[0m\] '
export PS2='> '

# Set environment variables
[ -f $HOME/.config/envrc ] && . $HOME/.config/envrc

# ==================================================
# Aliases
# ==================================================

alias config='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

alias ls='ls --color=auto'
alias l='ls -CF'
alias ll='ls -l'
alias la='ls -la'
alias l1='ls -1'
alias grep='grep --color=auto'
alias gtd='cd ~/drive/gtd'
alias finances='cd ~/drive/reference/finances'
alias dl='cd ~/downloads'
alias lg='lazygit'
# Also evaluate aliases preceded by 'sudo'
alias sudo='sudo '

alias theme='themeselect.sh'

# Config file aliases
alias cb="$EDITOR $HOME/.bashrc"
alias cbash="$EDITOR $HOME/.bashrc"
alias ci3="$EDITOR $HOME/.config/i3/config"
alias cw="$EDITOR $HOME/.config/i3/config"
alias cwm="$EDITOR $HOME/.config/i3/config"
alias cpoly="$EDITOR $HOME/.config/polybar/config"
alias cbar="$EDITOR $HOME/.config/polybar/config"
alias cpolybar="$EDITOR $HOME/.config/polybar/config"
alias vimrc="$EDITOR $HOME/.vim/vimrc"
alias cv="$EDITOR $HOME/.vim/vimrc"
alias cvim="$EDITOR $HOME/.vim/vimrc"


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
