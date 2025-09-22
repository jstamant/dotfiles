{ config, ... }:

{
  imports = [
    ./dev.nix
    ./gnome.nix
    ./jellyfin.nix
    ./printing.nix
  ];
}
