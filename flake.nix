{
  description = "navi flake";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?rev=1ffba9f2f683063c2b14c9f4d12c55ad5f4ed887";
    pythia-src.url = "github:tbidne/pythia";
    relative-time-src.url = "github:tbidne/relative-time";
    smart-math-src.url = "github:tbidne/smart-math";
  };
  outputs =
    { algebra-simple-src
    , flake-utils
    , nixpkgs
    , pythia-src
    , relative-time-src
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
