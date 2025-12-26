{ pkgs, ... }:

{
  # Enable the GNOME Desktop Environment
  services.gnome = {
    core-apps.enable = true; # Install the default GNOME apps
    games.enable = true;
  };
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Enable touchpad support (enabled default in most Desktop Environments)
  services.libinput.enable = true;
  # Added this to ensure my system extensions show up, but not sure why
  services.udev.packages = with pkgs; [ gnome-settings-daemon ];

  # Exclude some of the default GNOME packages
  environment.gnome.excludePackages = (with pkgs; [
    gnome-tour
    geary
  ]);

  environment.systemPackages = with pkgs; [
    gnome-tweaks
    gnomeExtensions.appindicator
    gnomeExtensions.blur-my-shell
  ];
}

