{ config, lib, pkgs, pkgs-unstable, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "max";
  boot.loader.efi.canTouchEfiVariables = true;

  # Networking
  networking.hostName = "navy";
  networking.networkmanager.enable = true;

  # Set your time zone
  time.timeZone = "America/Halifax";

  # Locale settings
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable sound with pipewire
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = false;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jstamant = {
    isNormalUser = true;
    description = "Justin St-Amant";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Set the battery charge threshold to 80% to preserve battery life
  services.power-profiles-daemon.enable = false; # Conflicts with tlp
  services.tlp = {
    enable = true;
    settings = {
      STOP_CHARGE_THRESH_BAT0 = 80;
      STOP_CHARGE_THRESH_BAT1 = 80;
    };
  };

  # Enable flakes
  nix.settings.experimental-features = [ "flakes" "nix-command" ];

  nixpkgs.config.allowUnfree = true;
  # Packages to install at the system-level
  environment.systemPackages = with pkgs; [
    alacritty
    audacity
    cheese
    coreutils-full
    deluge
    discord
    emacs-pgtk
    fd
    file # Utility for file info introspection
    fzf
    gcc # Definitely required to compile sqlite in Emacs
    git
    gimp
    gnupg
    google-chrome
    guile
    home-manager # TODO move this elsewhere?
    htop
    inkscape
    insync
    # insync-nautilus
    kicad
    libreoffice
    lutris
    mednafen
    mednaffe
    nil # nix language-server
    nixpkgs-fmt # nix formatter
    nodejs
    obsidian
    pandoc
    pdfarranger
    picard
    postman
    ranger
    ripgrep
    rofi
    ruffle
    rustup # might want to split this into its sub-packages
    shellcheck
    spotify
    stow
    tmux
    vim
    yt-dlp
    zig
    zls # zig language server
    zoom-us

    # DAW related
    ardour
    pkgs-unstable.decent-sampler # NOTE not working?
    pkgs-unstable.dexed
    dragonfly-reverb
    drumgizmo
    geonkick
    helm
    hydrogen
    lmms
    lsp-plugins
    non
    pkgs-unstable.sfizz-ui
    surge-XT
    vital
    x42-plugins
    x42-gmsynth
    x42-avldrums
    yoshimi
    zynaddsubfx
  ];

  kmonad.enable = true;
  gaming.enable = true;

  # Fonts to install at the system-level
  fonts.packages = with pkgs; [
    dejavu_fonts
    font-awesome
    noto-fonts
    # Since 25.05, I need this cludge to add all nerd-fonts
  ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # If considering changing this value, read this first:
  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "25.11"; # Did you read the comment?
}

