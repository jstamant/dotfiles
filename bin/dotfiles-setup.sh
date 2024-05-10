#!/bin/bash
# Run this script to install your dotfiles on a new machine

git clone --bare https://github.com/jstamant/dotfiles $HOME/.dotfiles
function config {
  git --git-dir=$HOME/.dotfiles --work-tree=$HOME $@
}
mkdir -p dotfiles-backup
config checkout
if [ $? = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} dotfiles-backup/{}
fi;
config checkout
#config config status.showUntrackedFiles no
