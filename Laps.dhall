let Laps = ./package.dhall

let nixEnv
    : Optional Laps.NixEnv
    = Some { srcFile = "default.nix", attr = None Text, clearEnv = False }

in  [ Laps.Command.single
        { name = "build"
        , shortDesc = "Build the project"
        , start =
            Laps.Start.Program
              { program = "cabal", arguments = [ "new-build" ] }
        , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
        , nixEnv = nixEnv
        }
    , Laps.Command.single
        { name = "format"
        , shortDesc = "Autoformat all source code"
        , start =
            Laps.Start.Script
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
    ]
