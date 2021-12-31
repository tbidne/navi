{
  description = "navi flake";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pythia-src = {
      url = "github:tbidne/pythia";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.refined-extras-src.follows = "refined-extras-src";
      inputs.refined-extras-src.inputs.flake-utils.follows = "flake-utils";
      inputs.refined-extras-src.inputs.nixpkgs.follows = "nixpkgs";
    };
    refined-extras-src = {
      url = "github:tbidne/refined-extras";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { flake-utils
    , nixpkgs
    , pythia-src
    , refined-extras-src
    , self
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
              optics-th = callHackage "optics-th" "0.4"
                { inherit optics-core; };
              pythia = final.callCabal2nix "pythia" pythia-src
                { inherit refined-extras; };
              refined-extras =
                final.callCabal2nix "refined-extras" refined-extras-src { };
            in
            {
              inherit
                optics-core
                optics-th
                pythia;
            };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
