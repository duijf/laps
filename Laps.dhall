let Types = ./Types.dhall

in  [ { name = "build"
      , shortDesc = "Build the project"
      , start =
          Types.Start.Script
            { interpreter = "/bin/bash"
            , contents =
                ''
                cabal new-build
                ''
            }
      , nixEnv = Some
          { srcFile = "default.nix", attr = None Text, clearEnv = False }
      , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
      }
    ]
