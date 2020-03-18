{ pkgs ? import ./nix/nixpkgs.nix {}
}:

{
  # Using laps in another project and downloaded the source using
  # e.g. `fetchGit` at a release tag? You need this attribute.
  laps = pkgs.haskellPackages.callPackage ./nix/laps.nix {};

  # Laps at a pinned commit. Ensures we can always use Laps to call
  # build tools while working on Laps itself (and potentially breaking
  # the build in the process). Probably only useful in development.
  # There are no guarantees about compatibility of `laps.nix` with
  # sources from arbitrary previous commits.
  laps-dev = pkgs.haskellPackages.callPackage ./nix/laps.nix {
    gitRev = "66eedb5b9f083690f48ef637a2f91c42335e0b42";
  };
}
