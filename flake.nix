{
  description = "Justin St-Amant's NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      inherit (self) outputs;
      system = "x86_64-linux";
      pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
    in {
      # NixOS configuration entrypoint
      # Available through 'sudo nixos-rebuild switch --flake ~/path/to/flake/#navy'
      nixosConfigurations.navy = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
          inherit pkgs-unstable;
        };
        modules = [
          ./hosts/navy/configuration.nix
          ./modules
          home-manager.nixosModules.home-manager {
            home-manager.users.jstamant = import ./home.nix;
            # Use system pkgs for HM for consistency
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };
}
