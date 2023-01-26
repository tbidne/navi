{
  description = "navi flake";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    #haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    byte-types = {
      url = "github:tbidne/byte-types";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    pythia = {
      url = "github:tbidne/pythia";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.byte-types.follows = "byte-types";
      inputs.monad-effects.follows = "monad-effects";
      inputs.smart-math.follows = "smart-math";
      inputs.time-conv.follows = "time-conv";
    };
    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    time-conv = {
      url = "github:tbidne/time-conv";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.monad-effects.follows = "monad-effects";
    };
  };
  outputs =
    { algebra-simple
    , bounds
    , byte-types
    , flake-compat
    , flake-parts
    , monad-effects
    , nixpkgs
    , pythia
    , relative-time
    , self
    , smart-math
    , time-conv
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc925";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
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
                algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                byte-types = final.callCabal2nix "byte-types" byte-types { };
                hedgehog = prev.hedgehog_1_2;
                monad-async =
                  final.callCabal2nix "monad-async"
                    "${monad-effects}/monad-async"
                    { };
                monad-exceptions =
                  final.callCabal2nix "monad-exceptions"
                    "${monad-effects}/monad-exceptions"
                    { };
                monad-fs =
                  final.callCabal2nix "monad-fs"
                    "${monad-effects}/monad-fs"
                    { };
                monad-ioref =
                  final.callCabal2nix "monad-ioref"
                    "${monad-effects}/monad-ioref"
                    { };
                monad-logger-namespace =
                  final.callCabal2nix "monad-logger-namespace"
                    "${monad-effects}/monad-logger-namespace"
                    { };
                monad-stm =
                  final.callCabal2nix "monad-stm"
                    "${monad-effects}/monad-stm"
                    { };
                monad-system-time =
                  final.callCabal2nix "monad-system-time"
                    "${monad-effects}/monad-system-time"
                    { };
                monad-terminal =
                  final.callCabal2nix "monad-terminal"
                    "${monad-effects}/monad-terminal"
                    { };
                monad-thread =
                  final.callCabal2nix "monad-thread"
                    "${monad-effects}/monad-thread"
                    { };
                monad-typed-process =
                  final.callCabal2nix "monad-typed-process"
                    "${monad-effects}/monad-typed-process"
                    { };
                package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
                pythia = final.callCabal2nix "pythia" pythia { };
                relative-time = final.callCabal2nix "relative-time" relative-time { };
                smart-math = final.callCabal2nix "smart-math" smart-math { };
                time-conv = final.callCabal2nix "time-conv" time-conv { };
                tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
              };
            };
        in
        {
          packages.default = mkPkg false false;
          devShells.default = mkPkg true true;
          devShells.ci = mkPkg true false;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
