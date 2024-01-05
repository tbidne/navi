{
  description = "navi flake";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    #haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    pythia = {
      url = "github:tbidne/pythia";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.si-bytes.follows = "si-bytes";
      inputs.monad-effects.follows = "monad-effects";
      inputs.smart-math.follows = "smart-math";
      inputs.time-conv.follows = "time-conv";
    };
    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    time-conv = {
      url = "github:tbidne/time-conv";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.monad-effects.follows = "monad-effects";
    };
  };
  outputs =
    inputs@{ flake-parts
    , monad-effects
    , nix-hs-utils
    , nixpkgs
    , self
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc963";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              hedgehog = prev.hedgehog_1_4;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_2_0;
              tasty-hedgehog = prev.tasty-hedgehog_1_4_0_2;
            } // nix-hs-utils.mkLibs inputs final [
              "algebra-simple"
              "bounds"
              "pythia"
              "relative-time"
              "si-bytes"
              "smart-math"
              "time-conv"
            ] // nix-hs-utils.mkRelLibs "${monad-effects}/lib" final [
              "effects-async"
              "effects-env"
              "effects-exceptions"
              "effects-fs"
              "effects-ioref"
              "effects-logger-ns"
              "effects-optparse"
              "effects-stm"
              "effects-time"
              "effects-terminal"
              "effects-thread"
              "effects-typed-process"
              "effects-unix-compat"
            ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "navi";
              root = ./.;
            };
          compilerPkgs = { inherit compiler pkgs; };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lintRefactor = nix-hs-utils.lintRefactor compilerPkgs;
          };
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
