{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.kmonad ];

  # Required groups for user access to /dev/input through kmonad
  users.users.jstamant.extraGroups = [ "input" ];
}
