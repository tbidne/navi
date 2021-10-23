{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/31ffc50c571e6683e9ecc9dbcbd4a8e9914b4497.tar.gz") { }
}:

let
  haskellDeps = ps: with ps; [
    haskell-language-server
    hlint
    implicit-hie
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = with pkgs; [
    git
    nixpkgs-fmt
    stack
    zlib.dev
    zlib.out
  ];
in
pkgs.haskell.lib.buildStackProject {
  name = "navi";

  buildInputs = otherDeps ++ haskellOtherDeps;

  ghc = ghc;
}

