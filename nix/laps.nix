{ haskellPackages
, lib
, mkDerivation
, watchexec
}:

mkDerivation {
  pname = "laps";
  version = "0.0.1.0-unreleased";

  src = lib.cleanSourceWith {
    filter = path: type:
      (lib.hasSuffix ".cabal" path || lib.hasSuffix ".hs" path || lib.hasSuffix ".dhall" path) &&
      lib.cleanSourceFilter path type;
    src = ../.;
  };

  isLibrary = false;
  isExecutable = true;

  # We could be, but aren't, more granular about this list by splitting
  # it into test, executable, and library dependencies. The `mkDerivation`
  # implementation will concatenate all of those into one big GHC package
  # DB, so we won't bother.
  executableHaskellDepends = import ./haskell-deps.nix haskellPackages;

  executableSystemDepends = [
    watchexec
  ];

  # I still need to choose something.
  license = "TODO";
}
