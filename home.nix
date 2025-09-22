# Look-up home-manager options at https://home-manager-options.extranix.com/
# Or in the manual at $ man home-configuration.nix.5

{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "jstamant";
  home.homeDirectory = "/home/jstamant";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/jstamant/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.alacritty.enable = true;
  # I used to use the one dark theme, but have been liking the default, lol
  # programs.alacritty.theme = "one_dark";
  programs.alacritty.settings = {
    window = {
      position = "None"; # Managed by the window manager (default)
      opacity = 0.95;
      startup_mode = "Maximized";
    };
    scrolling = {
      history = 10000;
    };
    font = {
      normal = {
        family = "DejaVu Sans Mono";
        style = "Regular";
      };
      size = 11.0;
    };
  };

  programs.git = {
    enable = true;
    userName = "Justin St-Amant";
    userEmail = "jstamant24@gmail.com";
    extraConfig = {
      core = {
        editor = "nvim"; # TODO use a variable here
        excludesFile = "${config.home.homeDirectory}/.config/git/ignore";
      };
      init.defaultBranch = "master";
    };
  };
  home.file.gitIgnore = {
    enable = true;
    target = ".config/git/ignore";
    text = ''
# Emacs projectile package project indicator file
.projectile

# Xcode project (insert poop emoji)
**/project.xcworkspace/xcuserdata/*
'';
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
# Enforce tmux to work in 256 colors, often required by interactive programs, like neovim
set -g default-terminal "tmux-256color"
# Fix colors on alacritty, and hopefully for all terminals
set -ag terminal-features ',alacritty:RGB'
set -ag terminal-features ',*:RGB'

# Enable focus events for neovim
set -g focus-events on

# Prevent exiting tmux on C-d, unless 3 C-d are sent consecutively
#set-environment -g 'IGNOREEOF' 2

# Configure scroll settings
set -g history-limit 10000
set -g mouse on # TODO consider disabling this
set -g mode-keys vi # TODO change to emacs, and learn it

# Disable escape sequence timer (required for vim)
set -s escape-time 0

#
# STATUS LINE SETTINGS
#

set -g status-fg default
set -g status-bg blue
set -g status-left  '[#S] '
#set -g status-right \'\'
set -g status-right '#(date "+%F %R ")'
set -g status-interval 15

# Start window numbering from 1
set -g base-index 1
# Renumber windows so they stay in order
set -g renumber-windows on

#
# KEYBINDINGS
#

# Change the prefix key to backslash '\'
unbind C-b
set -g prefix C-'\'
bind C-'\' send-prefix
# Split and vertical split like in vim
bind-key 'C-s' split-window -v
bind-key 'C-v' split-window -h
# Navigate panes with vim h,j,k,l
bind-key 'h' select-pane -L
bind-key 'j' select-pane -D
bind-key 'k' select-pane -U
bind-key 'l' select-pane -R
# Toggling to last window
bind-key 'C-b' last-window
bind-key 'C-l' last-window
bind-key C-'\' last-window
# Copy and paste-buffer keys
set -g set-clipboard off
bind-key -T copy-mode-vi 'v' send-keys -X begin-selection
# On Wayland
bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "wl-copy && wl-paste -n | wl-copy -p"
bind-key p run "wl-paste -n | tmux load-buffer - ; tmux paste-buffer"
# On Xorg
#bind-key -T copy-mode-vi 'y' send-keys -X copy-pipe-and-cancel "xclip -selection clipboard -i"
#bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "xclip -i -sel clip > /dev/null"
#bind-key p run "xclip -o -sel clip | tmux load-buffer - ; tmux paste-buffer"
'';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
