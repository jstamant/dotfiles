#!/usr/bin/bash
# Open one of my programming projects in a tmux session

PROJECT_PATH=$(find $HOME/programming/* -maxdepth 0 | fzf)
if [[ $? != 0 ]]; then exit 1; fi
PROJECT=$(basename $PROJECT_PATH)

if ! $(tmux has-session -t $PROJECT 2> /dev/null); then
  tmux new-session -d -s $PROJECT -c $PROJECT_PATH
fi

if [[ $TMUX ]]; then
  tmux switch-client -t $PROJECT
else
  tmux attach-session -t $PROJECT
fi
