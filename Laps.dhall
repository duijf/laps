[ { name = "build"
  , shortDesc = "Build the project"
  , program = "cabal"
  , arguments = [ "new-build" ]
  , nixEnv = Some { srcFile = "default.nix", attr = None Text, pure = False }
  , watchExtensions = [ ".cabal", ".hs" ]
  }
]
