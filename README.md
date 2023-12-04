# Justin St-Amant's Dotfiles

These are my personal configuration files. Please feel free to use them or browse them.

The goal of my dotfiles are to provide me with a portable user experience across
all the machines that I use. At home, I use an Arch Linux laptop. I'm currently
in the process of refactoring the deployment of these files using Ansible and
GNU stow.

I run my system lean. I use a window manager setup, i.e. I pick and choose
every single component that goes into my system. Right now I'm using `awesome`.

I have some utility scripts in the [`bin`](/bin) folder.
Some of my configuration files might depend on these.

If you're looking for my custom Emacs configuration, I've moved it to
[its own repository](https://github.com/jstamant/.emacs.d).
My Spacemacs configuration is found in this repository, though.

## Current Setup - December 2023

![Screenshot](/.assets/2023-12-13-160853-scrot.png)

Here's what I'm running that I have configuration files for:

| Software        | Function/purpose                           |
| :-------------- | :----------------------------------------- |
| **awesome**     | Window manager                             |
| **awful.wibar** | Status bar (builtin with awesome)          |
| **emacs**       | Text editor                                |
| **alacritty**   | Terminal emulator                          |
| **naughty**     | Notification daemon (builtin with awesome) |
| **ranger**      | Terminal file-manager                      |
| **sxhkd**       | Hotkey daemon                              |
| **autorandr**   | Automatic switching of display setups      |
| picom           | Compositor (not currently being used)      |


## Installation

To clone the repository to your home directory and pull in the files, run the
following commands in your `$HOME` directory:

```
$ git init
$ git remote add origin https://github.com/jstamant/dotfiles.git
$ git pull origin master
```

## Older setups

I've tried quite a few combinations of window managers and software over the
years. I have configuration files for these pieces of software that I no longer
use:

- xmonad, window manager
- bspwm, window manager
- dwm, window manager
- i3, window manager
- openbox, window manager
- dunst, notification daemon
- feh, wallpaper-setter
- trayer, system tray
- xmobar, status bar
- polybar, status bar
- tint2, status bar
- xbindkeys, hotkey daemon
- xterm, terminal emulator
- rxvt-unicode, terminal emulator
- st, terminal emulator
- kitty, terminal emulator

Setup from 2022:

![Screenshot](/.assets/2022-02-09-192945-scrot.png)

Setup from 2020: looks like I was trying bspwm as my window manager, at some point!

![Screenshot](/.assets/2020-02-15-133338-scrot.png)
