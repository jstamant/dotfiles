#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Standard Arch Linux PS1 in blue
# [user@hostname ~]$ ls -la
export PS1='\[\e[1;34m\][\u@\h \W]\$\[\e[0m\] '
export PS2='> '

# Set environment variables
[ -f $HOME/.config/envrc ] && . $HOME/.config/envrc

# Aliases
alias ls='ls --color=auto'
alias l='ls -CF'
alias ll='ls -l'
alias la='ls -la'
alias l1='ls -1'
alias grep='grep --color=auto'
# TODO make this determined by my home dir as per nix
alias dl='cd ~/downloads'
alias dot="cd ~/.dotfiles"
alias lg='lazygit'
alias sudo='sudo ' # Evaluate aliases preceded by sudo
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias op='open-project.sh'
