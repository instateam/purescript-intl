{ pkgs ? import <nixpkgs> {}
}:

with pkgs.lib;

let
  nodejs = pkgs.nodejs-14_x;

  # pulling apart the src so that we can override the NodeJS version
  easy-ps-src = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "734ca9c00038f4b549bf8fc58eb65e08a87e9d56";
    sha256 = "sha256-rHbjHTN2obUkMKQXdIHZBnnCO6mp7BP7SZ2tUlp9wrI=";
  };

  easy-ps = import easy-ps-src { inherit pkgs; } // {
    psa = import "${easy-ps-src}/psa" { inherit pkgs nodejs; };
    pscid = import "${easy-ps-src}/pscid" { inherit pkgs nodejs; };
  };
in
{
  name = "purescript-intl";

  project = { };

  shell = pkgs.mkShell {
    name = "purescript-intl";

    nativeBuildInputs = with pkgs.buildPackages; [
      # node
      nodejs
      (yarn.override { nodejs = nodejs; })

      # purescript
      easy-ps.spago
      easy-ps.purs-0_13_8
      easy-ps.psc-package
      easy-ps.purty
      easy-ps.psa
      easy-ps.pscid

      # dhall
      dhall
      dhall-bash
    ];
  };
}
