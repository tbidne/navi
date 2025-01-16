{
  description = "navi flake";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
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
    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
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
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.smart-math.follows = "smart-math";
    };
    pythia = {
      url = "github:tbidne/pythia";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.monad-effects.follows = "monad-effects";
      inputs.si-bytes.follows = "si-bytes";
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
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.monad-effects.follows = "monad-effects";
      inputs.smart-math.follows = "smart-math";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      monad-effects,
      nix-hs-utils,
      nixpkgs,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                # The latest dbus in nixpkgs, dbus_1_3_8, has compilation
                # problems with doJailbreak, which we need for filepath.
                # Hence the direct call here.
                dbus = (
                  final.callHackageDirect {
                    pkg = "dbus";
                    ver = "1.3.10";
                    sha256 = "sha256-Of2aSeCzi5+LHyR+6aPxlCW/Oa8QfjwKrIieqs2h6YE=";
                  } { }
                );

                network = prev.network_3_2_4_0;

                path = hlib.dontCheck prev.path_0_9_6;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "pythia"
                "relative-time"
                "si-bytes"
                "smart-math"
                "time-conv"
              ]
              // nix-hs-utils.mkRelLibs "${monad-effects}/lib" final [
                "effects-async"
                "effects-env"
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
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "navi";
              root = ./.;

              # TODO: Once hlint is back to working with our GHC we can
              # use nix-hs-utils.mkDevTools ++ otherDeps.
              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            #lint = nix-hs-utils.lint compilerPkgs;
            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    };
}
