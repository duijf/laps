{ pkgs ? import ./nix/nixpkgs.nix {}
}:

rec {
  # Using laps in another project and downloaded the source using
  # e.g. `fetchGit` at a release tag? You need this attribute.
  laps = pkgs.haskellPackages.callPackage ./nix/laps.nix {
    inherit libsys;
  };

  # Laps at a pinned commit. Ensures we can always use Laps to call
  # build tools while working on Laps itself (and potentially breaking
  # the build in the process). Probably only useful in development.
  # There are no guarantees about compatibility of `laps.nix` with
  # sources from arbitrary previous commits.
  laps-dev = pkgs.haskellPackages.callPackage ./nix/laps.nix {
    gitRev = "6a9c5d53b8603e0574b659c8eab55e8213737ed5";
    inherit libsys;
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
