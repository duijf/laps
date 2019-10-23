let
  pkgs = import ./nix/nixpkgs.nix;
in
  pkgs.buildEnv {
    name = "laps-devenv";
    paths = [
      pkgs.latest.rustChannels.stable.rust
    ];
  }
