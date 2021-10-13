{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/31ffc50c571e6683e9ecc9dbcbd4a8e9914b4497.tar.gz") { }
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
