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
    , { name = "typecheck"
      , shortDesc = "Typecheck files"
      , startOrder =
          Laps.parallel
            [ Laps.single
                { alias = "dhall"
                , executable =
                    Laps.program
                      { program = "dhall"
                      , arguments = [ "--ascii", "--file", "package.dhall" ]
                      }
                , nixEnv = nixEnv
                , watchExtensions = [] : List Text
                }
            , Laps.single
                { alias = "haskell"
                , executable =
                    Laps.program
                      { program = "cabal"
                      , arguments =
                        [ "build"
                        , "--ghc-options=\"-fforce-recomp -fno-code\""
                        ]
                      }
                , nixEnv = nixEnv
                , watchExtensions = [] : List Text
                }
            ]
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
