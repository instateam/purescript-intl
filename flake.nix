{
  description = "purescript-intl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:toastal/easy-purescript-nix/upgrade-npm-packages";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    let
      ourSystems = [ "x86_64-darwin" "x86_64-linux" ];
    in
    flake-utils.lib.eachSystem ourSystems (system:
      let
        name = "purescript-intl";

        pkgs = import nixpkgs { inherit system; };

        nodejs = pkgs.nodejs-14_x;

        easy-ps = import easy-purescript-nix { inherit pkgs; } // {
          psa = import "${easy-purescript-nix}/psa" { inherit pkgs nodejs; };
          pscid = import "${easy-purescript-nix}/pscid" { inherit pkgs nodejs; };
        };

        buildInputs = with pkgs; [
          # dhall
          dhall
          dhall-bash
          dhall-json
        ];

        nativeBuildInputs = with pkgs.buildPackages; [
          # node
          nodejs

          # purescript
          easy-ps.spago
          easy-ps.purs-0_14_2
          easy-ps.psc-package
          easy-ps.purty
          easy-ps.zephyr
          easy-ps.psa
          easy-ps.pscid

          # auxiliary
          dash
        ];
      in
      rec {
        devShell = pkgs.mkShell
          {
            inherit name nativeBuildInputs buildInputs;
            # inputsFrom = builtins.attrValues self.packages.${system};
          };
      }
    );
}
