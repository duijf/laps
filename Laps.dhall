[ { name = "build"
  , shortDesc = "Build the project"
  , start = { program = "cabal", arguments = [ "new-build" ] }
  , nixEnv = Some { srcFile = "default.nix", attr = None Text, clearEnv = False }
  , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
  }
]
