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
    };
    environment.systemPackages = with pkgs; [
      heroic
      openmw
      openrct2
    ];
  };
}
