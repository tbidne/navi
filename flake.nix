{
  description = "navi flake";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    byte-types-src.url = "github:tbidne/byte-types";
    env-guard-src.url = "github:tbidne/env-guard";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pythia-src.url = "github:tbidne/pythia";
    relative-time-src.url = "github:tbidne/relative-time";
    smart-math-src.url = "github:tbidne/smart-math";
  };
  outputs =
    { algebra-simple-src
    , byte-types-src
    , env-guard-src
    , flake-utils
    , nixpkgs
    , pythia-src
    , relative-time-src
    , self
    , smart-math-src
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc902";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "navi";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              pkgs.zlib
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            byte-types =
              final.callCabal2nix "byte-types" byte-types-src { };
            env-guard =
              final.callCabal2nix "env-guard" env-guard-src { };
            package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
            pythia = final.callCabal2nix "pythia" pythia-src { };
            relative-time = final.callCabal2nix "relative-time" relative-time-src { };
            smart-math =
              final.callCabal2nix "smart-math" smart-math-src { };
            tasty-hedgehog = prev.tasty-hedgehog_1_2_0_0;
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
