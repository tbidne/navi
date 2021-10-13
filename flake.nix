{
  description = "navi flake";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
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
                implicit-hie
                ormolu
                pkgs.nixpkgs-fmt
                pkgs.zlib
              ]);
          };
      in
      {
        defaultPackage = mkPkg false;

        devShell = mkPkg true;
      });
}
