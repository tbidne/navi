{
  description = "navi flake";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    byte-types-src.url = "github:tbidne/byte-types";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
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
    , flake-compat
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
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];
      ghc-version = "ghc923";
      compiler = pkgs.haskell.packages."${ghc-version}";
      mkPkg = returnShellEnv: withDevTools:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "navi";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++
                (if withDevTools then devTools compiler else [ ]));
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
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
