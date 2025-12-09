{ lib, ... }:

let
  modules_dir = builtins.readDir ../modules;
  module_file_names = (builtins.filter
    (name: lib.hasSuffix ".nix" name && name != "default.nix")
    (builtins.attrNames modules_dir));
in
{
  # Import all .nix modules in the modules directory
  imports = map (name: ./${name}) module_file_names;
}
