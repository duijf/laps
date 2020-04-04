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
    gitRev = "9fd36fbc62e8479ee0e498fd40502e2560c5a131";
  };

  libsys = pkgs.stdenv.mkDerivation {
    name = "libsys";

    phases = ["buildPhase" "installPhase"];

    buildPhase = ''
      cp ${./sys.rs} sys.rs
      ${pkgs.rustc}/bin/rustc --crate-type=cdylib sys.rs
    '';

    installPhase = ''
      ${pkgs.tree}/bin/tree
      mkdir -p $out/lib
      cp libsys.so $out/lib
    '';
  };
}
