{ pkgs, ... }:
{
  # TODO ...really need to move this to home-manager
  # Enable docker for local development
  virtualisation.docker.enable = true;
  users.users.jstamant.extraGroups = [ "docker" ];


  environment.systemPackages = with pkgs; [
    dbeaver-bin
  ];
}
