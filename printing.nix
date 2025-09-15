{ pkgs, ... }:

{
  # Enable CUPS to print documents
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      cups-filters
      cups-browsed
    ];
  };
  # Added to apparently support network printer auto-discovery
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  # Enable SANE for scanner support
  hardware.sane.enable = true;
  # sane-airscan is apparently a third-party package with better support
  # than the default escl backend
  hardware.sane.extraBackends = [ pkgs.sane-airscan ];
  hardware.sane.disabledDefaultBackends = [ "escl" ];

  # Required groups for printing and scanning
  users.users.jstamant.extraGroups = [ "lp" "scanner" ];
}
