{ pkgs ? import ./nix/nixpkgs.nix {}
}:

{
  laps = pkgs.haskellPackages.callPackage ./nix/laps.nix {};
}
