{ pkgs ?  (import ./nix/nixpkgs.nix {})
}:

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages (import ./nix/haskell-deps.nix);
  release = (import ./release.nix) { inherit pkgs; };
in
  buildEnv {
    name = "laps-devenv";
    paths = [
      cabal-install
      ghc
      release.laps-dev
      stylish-haskell
      watchexec
      rustc
      release.libsys
    ];
  }
