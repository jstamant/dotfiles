{ config, lib, pkgs, ... }:

{
  # Required to open jellyfin to local network at default port 8096/8920
  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };
  # jellyfin must be started with `sudo systemctl start jellyfin`
  systemd.services.jellyfin.wantedBy = lib.mkForce [ ];

  # TODO to move this to home-manager
  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];

  # TODO to move this to home-manager
  # jellyfin added to easily copy over media to /srv/jellyfin
  users.users.jstamant = {
    extraGroups = [ "jellyfin" ];
  };
}
