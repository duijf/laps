let Laps = ./package.dhall

let nixEnv
    : Optional Laps.NixEnv
    = Some { srcFile = "default.nix", attr = None Text, clearEnv = False }

in  [ { name = "build"
      , shortDesc = "Build the project"
      , startOrder =
          Laps.single
            { executable =
                Laps.program { program = "cabal", arguments = [ "new-build" ] }
            , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
            , nixEnv = nixEnv
            , alias = "cabal"
            }
      }
    , { name = "format"
      , shortDesc = "Autoformat all source code"
      , startOrder =
          Laps.single
            { alias = "format"
            , executable =
                Laps.script
                  { interpreter = "/bin/bash"
                  , contents =
                      ''
                      fd -e dhall --exec dhall --ascii format --inplace {}
                      fd -e hs --exec stylish-haskell --inplace {}
                      ''
                  }
            , nixEnv = nixEnv
            , watchExtensions = [] : List Text
            }
      }
    ]
