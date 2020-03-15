let
  pkgs = import ./nix/nixpkgs.nix {};
  haskell = pkgs.haskellPackages.ghcWithPackages (ps: [
    ps.containers
    ps.typed-process
  ]);
in
  pkgs.buildEnv {
    name = "laps-devenv";
    paths = with pkgs; [
      cabal-install
      haskell
    ];
  }
