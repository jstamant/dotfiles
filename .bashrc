# ==================================================
# ~/.bashrc
# ==================================================

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Standard Arch Linux PS1 in blue
# [jstamant@t460 ~]$ ls -la
export PS1='\[\e[1;34m\][\u@\h \W]\$\[\e[0m\] '
export PS2='> '

# Set vim as primary editor
export EDITOR='vim'
export VISUAL='vim'

# Explicitly set pager to less
export PAGER='less'

# Set other non-standard variables for my own scripts
export LAUNCHER='dmenu'

# Custom PATH additions
export PATH=$PATH:~/bin

# ==================================================
# Aliases
# ==================================================

alias ls='ls --color=auto'
alias l='ls -CF'
alias ll='ls -l'
alias la='ls -la'
alias l1='ls -1'
alias grep='grep --color=auto'
alias gtd='cd ~/drive/gtd'
alias finances='cd ~/drive/reference/finances'
alias dl='cd ~/downloads'
# Also evaluate aliases preceded by 'sudo'
alias sudo='sudo '

alias theme='themeselect.sh'

alias 2019="$EDITOR $HOME/drive/reference/finances/2019-finances.ledger"

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
