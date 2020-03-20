let Types = ./Types.dhall

in  [ { name = "build"
      , shortDesc = "Build the project"
      , start =
          Types.Start.Program { program = "cabal", arguments = [ "new-build" ] }
      , nixEnv = Some
          { srcFile = "default.nix", attr = None Text, clearEnv = False }
      , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
      }
    , { name = "format"
      , shortDesc = "Autoformat all source code"
      , start =
          Types.Start.Script
            { interpreter = "/bin/bash"
            , contents =
                ''
                fd -e dhall --exec dhall --ascii format --inplace {}
                fd -e hs --exec stylish-haskell --inplace {}
                ''
            }
      , nixEnv = Some
          { srcFile = "default.nix", attr = None Text, clearEnv = False }
      , watchExtensions = [] : List Text
      }
    ]
