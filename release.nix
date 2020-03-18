{ pkgs ? import ./nix/nixpkgs.nix {}
}:

{
  laps = pkgs.haskellPackages.callPackage ./nix/laps.nix {};
  lapsDev = pkgs.haskellPackages.callPackage ./nix/laps.nix { devBuild = true; };
}
