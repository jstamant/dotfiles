# TODO delete files in /etc/nixos once I have this going??????
{
  # TODO change description
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: let
    inherit (self) outputs;
  in {
    # NixOS configuration entrypoint
    # Available through 'sudo nixos-rebuild switch --flake .#navy'
    nixosConfigurations.navy = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs outputs;};
      modules = [
        ./hosts/navy/configuration.nix
      ];
    };
    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager switch --flake .#jstamant@navy'
    homeConfigurations = {
      "jstamant@navy" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires a 'pkgs' instance
        extraSpecialArgs = {inherit inputs outputs;};
        modules = [
          ./home.nix
        ];
      };
    };
  };
}
