# SYNOPSIS
#
#   Pinned nixpkgs version.
#
#   Enabled reproducibility, so users don't rely on the NIX_PATH and global
#   state. This also allows this repo to be used when only Nix has been
#   installed through the bin/bootstrap-nix.py installer and nixpkgs isn't
#   available.
#
# USAGE
#
#   let
#     # You can pass in arbtrary arguments to nixpkgs if you want.
#     pkgs = import ./path/to/here/nixpkgs.nix {};
#   in
#     pkgs.stdenv.mkDerviation { .. }
#
# UPDATING
#
#   Replace the value of `nixpkgsRev` with the latest commit on GitHub and
#   build something depending on it. Then read the error message and update the
#   sha256 value.
let
  nixpkgsRev = "7a53b1cbe79d91167874eb4f0d4ccc69c2e45007";
  nixpkgsTar = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "sha256:1mwxipvyrwfbl0z6mghv0gr6z61awidnpld1p8h4fwjfifv0dn2j";
  };

  mozOverlayRev = "d46240e8755d91bc36c0c38621af72bf5c489e13";
  mozOverlayTar = fetchTarball {
    url = "https://github.com/mozilla/nixpkgs-mozilla/archive/${mozOverlayRev}.tar.gz";
    sha256 = "sha256:0icws1cbdscic8s8lx292chvh3fkkbjp571j89lmmha7vl2n71jg";
  };
in
  import "${nixpkgsTar}/default.nix" {
    overlays = [
      (import "${mozOverlayTar}/default.nix")
    ];
  }
