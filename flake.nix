{
  description = "navi flake";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    byte-types-src.url = "github:tbidne/byte-types";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pythia-src.url = "github:tbidne/pythia";
    relative-time-src.url = "github:tbidne/relative-time";
    smart-math-src.url = "github:tbidne/smart-math";
    time-conv-src.url = "github:tbidne/time-conv";
  };
  outputs =
    { algebra-simple-src
    , byte-types-src
    , flake-utils
    , nixpkgs
    , pythia-src
    , relative-time-src
    , self
    , smart-math-src
    , time-conv-src
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc923";
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
            package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
            pythia = final.callCabal2nix "pythia" pythia-src { };
            relative-time = final.callCabal2nix "relative-time" relative-time-src { };
            smart-math =
              final.callCabal2nix "smart-math" smart-math-src { };
            time-conv =
              final.callCabal2nix "time-conv" time-conv-src { };
            tasty-hedgehog = prev.tasty-hedgehog_1_2_0_0;
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
