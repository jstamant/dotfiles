# Doome Emacs Configuration

Just some notes for installing, updating, and syncing Doom Emacs.

Since the Doom binaries should be in your PATH, you can simply run Doom-commands
like:

``` sh
doom install
doom sync
doom doctor
```

## Installing Doom Emacs

First, make sure that your Doom dotfiles are installed.

``` sh
cd ~/.dotfiles
stow -R doom
```

Then, enter this to install Doom Emacs.

``` sh
git clone https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

Then you'll want to run `doom doctor` to see what else needs to be installed.

``` sh
~/.emacs.d/bin/doom doctor
```

And don't forget to run `doom sync` to make sure your config is synced before
running it for the first time.

``` sh
~/.emacs.d/bin/doom sync
```
