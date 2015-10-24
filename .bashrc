#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Standard Arch Linux PS1 in blue
PS1='\[\e[1;34m\][\u@\h \W]\$\[\e[0m\] '

# Aliases
alias ls='ls --color=auto'
alias l='ls -CF'
alias ll='ls -l'
alias la='ls -la'

alias grep='grep --color=auto'

# Also evaluate aliases preceded by 'sudo'
alias sudo='sudo '

# emacs as primary editor
alias emc='emacsclient -t'
EDITOR='emacsclient -t'
