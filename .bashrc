#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Standard Arch Linux PS1 in blue
export PS1='\[\e[1;34m\][\u@\h \W]\$\[\e[0m\] '
export PS2='> '

# Set nano as primary editor
EDITOR='nano'
VISUAL='emacsclient'

#
# ALIASES
#
alias ls='ls --color=auto'
alias l='ls -CF'
alias ll='ls -l'
alias la='ls -la'
alias grep='grep --color=auto'
# Also evaluate aliases preceded by 'sudo'
alias sudo='sudo '
# Emacs over terminal
alias emt='emacsclient -t'
# Send file to running Emacs server
alias emc='emacsclient -n'
