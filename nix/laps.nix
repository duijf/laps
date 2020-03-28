{ haskellPackages
, lib
, mkDerivation
, watchexec
, gitRev ? ""
}:

mkDerivation {
  pname = "laps";
  version = "0.0.1.0-unreleased";

  src =
    let
      releaseSrc = lib.cleanSourceWith {
        filter = path: type:
          (lib.hasSuffix ".cabal" path || lib.hasSuffix ".hs" path || lib.hasSuffix ".dhall" path) &&
          lib.cleanSourceFilter path type;
        src = ../.;
      };

      # A previously known good commit of the repo, so we can keep using Laps
      # itself to develop Laps. This permits introducing build errors in the
      # working copy of the repository while still having Laps available for
      # our build commands.
      devSrc =
        fetchGit {
          # We cannot use `url = ../.;`. That makes Nix complain.
          url = "https://github.com/duijf/laps.git";
          name = "laps-git";
          ref = "haskell";
          rev = gitRev;
        };
    in
      if gitRev == "" then releaseSrc else devSrc;

  isLibrary = false;
  isExecutable = true;

  # We could be, but aren't, more granular about this list by splitting
  # it into test, executable, and library dependencies. The `mkDerivation`
  # implementation will concatenate all of those into one big GHC package
  # DB, so we won't bother.
  executableHaskellDepends = import ./haskell-deps.nix haskellPackages;

  executableToolDepends = [
    watchexec
  ];

  # I still need to choose something.
  license = "TODO";
}
