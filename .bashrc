#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# emacs as primary editor
alias emc='emacsclient -t'
EDITOR='emacsclient -t'
