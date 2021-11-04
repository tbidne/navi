{
  description = "navi flake";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.simple-algebra-src = {
    url = "github:tbidne/simple-algebra";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.smart-data-src = {
    url = "github:tbidne/smart-data";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.simple-algebra-src.follows = "simple-algebra-src";
  };
  inputs.system-info-src = {
    url = "github:tbidne/system-info";
    inputs.flake-utils.follows = "flake-utils";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.simple-algebra-src.follows = "simple-algebra-src";
    inputs.smart-data-src.follows = "smart-data-src";
  };
  outputs =
    { flake-utils
    , nixpkgs
    , self
    , simple-algebra-src
    , smart-data-src
    , system-info-src
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc8107";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "navi";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              cabal-plan
              haskell-language-server
              hlint
              ghcid
              implicit-hie
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
          overrides = final: prev: with pkgs.haskellPackages;
            let
              optics-core = callHackage "optics-core" "0.4" { };
              smart-data = final.callCabal2nix "smart-data" smart-data-src {
                simple-algebra =
                  final.callCabal2nix "simple-algebra" simple-algebra-src { };
              };
            in
            {
              optics-core = optics-core;
              optics-th = callHackage "optics-th" "0.4"
                { inherit optics-core; };
              smart-data = smart-data;
              system-info = final.callCabal2nix "system-info" system-info-src
                { inherit smart-data; };
            };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
