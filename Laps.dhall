let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

let Laps = ./package.dhall

let nixEnv
    : Optional Laps.NixEnv
    = Some { srcFile = "default.nix", attr = None Text, clearEnv = False }

let build
    : Bool -> Laps.Command
    =     \(release : Bool)
      ->  { name =
              Prelude.Text.concat [ "build", if release then "-rel" else "" ]
          , shortDesc =
              Prelude.Text.concat
                [ "Build the project", if release then " (release)" else "" ]
          , startOrder =
              Laps.single
                { executable =
                    Laps.script
                      { interpreter = "/bin/bash"
                      , contents =
                          ''
                          cabal configure --extra-lib-dirs=$(nix-build release.nix -A libsys)/lib
                          cabal new-build ${if release then "-frelease" else ""}
                          ''
                      }
                , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
                , nixEnv = nixEnv
                , alias = "cabal"
                }
          }

in  [ build True
    , build False
    , { name = "test"
      , shortDesc = "Haskell tests"
      , startOrder =
          Laps.single
            { alias = "cabal"
            , executable =
                Laps.program { program = "cabal", arguments = [ "test" ] }
            , nixEnv = nixEnv
            , watchExtensions = [ ".cabal", ".hs", ".dhall" ]
            }
      }
    , { name = "test-accept"
      , shortDesc = "Haskell tests (accept changes to golden tests)"
      , startOrder =
          Laps.single
            { alias = "cabal"
            , executable =
                Laps.program { program = "cabal", arguments = [ "test", "--test-option=--accept" ] }
            , nixEnv = nixEnv
            , watchExtensions = [] : List Text
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
