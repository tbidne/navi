{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/7e5bf3925f6fbdfaf50a2a7ca0be2879c4261d19.tar.gz") { }
}:

let
  haskellDeps = ps: with ps; [
    cabal-install
    cabal-fmt
    cabal-plan
    hlint
    implicit-hie
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = with pkgs; [
    nixpkgs-fmt
    zlib
  ];
in
pkgs.mkShell {

  buildInputs =
    [ ghc ]
    ++ haskellOtherDeps
    ++ otherDeps;
}
