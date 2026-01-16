{ config, lib, pkgs, ... }:

{
  options.gaming = {
    enable = lib.mkEnableOption "Whether to enable gaming";
  };

  config = lib.mkIf config.gaming.enable {
    programs.steam = {
      enable = true;
      extraCompatPackages = [ pkgs.proton-ge-bin];
      protontricks.enable = true;
      remotePlay.openFirewall = true; # Enable Steam Link
    };
    environment.systemPackages = with pkgs; [
      heroic
      openmw
      openrct2
      openttd
      openttd-ttf
    ];
    home-manager.users.jstamant = { pkgs, ... }: {
      programs.retroarch = {
        enable = true;
        cores.snes9x.enable = true;
      };
    };
  };
}
