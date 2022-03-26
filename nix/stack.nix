let
  compilerVersion = "ghc902";
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  pkgs = import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
      sha256 = lock.nodes.nixpkgs.locked.narHash;
    })
    { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.haskell.lib.buildStackProject {
  name = "navi";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ];

  ghc = compiler.ghc;
}
