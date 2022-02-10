# Justin St-Amant's Dotfiles

These are my personal configuration files. Please feel free to use them or
browse them.

The goal of my dotfiles are to provide me with a portable user experience across
all the machines that I use. At home, I currently use an Arch Linux laptop and
an Arch Linux desktop dual-booted with Windows. At work, I use a Windows laptop.
The experience across my Arch Linux machines must be near identical. My Windows
machines only need my Emacs configuration.

I run my system bare-bones. I use a window manager setup, i.e. I pick and choose
every single component that goes into my system. Right now I'm using `xmonad`. I
don't jump around often.

I have some utility scripts in the [`bin`](/bin) folder. Some of my
configuration files might depend on these, like `refreshbar.sh`, which is a
script that my `dwm` setup requires.

## Screenshots

Current setup (2022)!

![Screenshot](/.assets/2022-02-09-192945-scrot.png)

Previous setup (2020): looks like I was trying bspwm as my window manager, at some point!

![Screenshot](/.assets/2020-02-15-133338-scrot.png)

## Installation

To clone the repository to your home directory and pull in the files, run the
following commands in your `$HOME` directory:

```
$ git init
$ git remote add origin https://github.com/jstamant/dotfiles.git
$ git pull origin master
```

## My current setup

Here's what I'm running that I have configuration files for:

- xmonad, window manager
- xmobar, status bar
- emacs, text editor
- kitty, terminal emulator
- st, terminal emulator
- dunst, notification daemon
- autorandr, automatic switching of display setups
- feh, wallpaper-setter
- picom, compositor
- ranger, terminal file-manager
- sxhkd, hotkey daemon
- trayer, system tray
- zathura, document viewer with vim keybindings

## Old setup

I've tried quite a few combinations of window managers and software over the
years. I have configuration files for these pieces of software that I no longer
use:

- bspwm
- dwm
- i3
- openbox
- polybar
- rxvt-unicode
- tint2
- xbindkeys
- xterm
