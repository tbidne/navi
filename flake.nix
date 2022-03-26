{
  description = "navi flake";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pythia-src.url = "github:tbidne/pythia";
    refined-extras-src.url = "github:tbidne/refined-extras";
    smart-math-src.url = "github:tbidne/smart-math";
  };
  outputs =
    { algebra-simple-src
    , flake-utils
    , nixpkgs
    , pythia-src
    , refined-extras-src
    , self
    , smart-math-src
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
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
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            optics-core = final.optics-core_0_4;
            optics-th = final.optics-th_0_4;
            pythia = final.callCabal2nix "pythia" pythia-src { };
            refined-extras =
              final.callCabal2nix "refined-extras" refined-extras-src { };
            smart-math =
              final.callCabal2nix "smart-math" smart-math-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
