{ pkgs ?  (import ./nix/nixpkgs.nix {})
}:

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages (import ./nix/haskell-deps.nix);
in
  buildEnv {
    name = "laps-devenv";
    paths = [
      cabal-install
      ghc
      watchexec
    ];
  }
