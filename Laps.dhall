let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

let Laps = ./package.dhall

in  [ Laps.simpleScript
        { name = "build"
        , shortDesc = "Build the Haskell code"
        , interpreter = "/bin/bash"
        , contents =
            ''
            cabal configure --extra-lib-dirs=$(nix-build release.nix -A libsys)/lib
            cabal new-build
            ''
        , envVars = [] : List Laps.EnvVar
        }
    , Laps.simpleScript
        { name = "build-release"
        , shortDesc = "Build the Haskell code (release)"
        , interpreter = "/bin/bash"
        , contents =
            ''
            cabal configure --extra-lib-dirs=$(nix-build release.nix -A libsys)/lib
            cabal new-build -frelease
            ''
        , envVars = [] : List Laps.EnvVar
        }
    , Laps.simpleProgram
        { name = "test"
        , shortDesc = "Haskell tests"
        , program = "cabal"
        , arguments = [ "test" ]
        , envVars = [] : List Laps.EnvVar
        }
    , Laps.simpleProgram
        { name = "test-accept"
        , shortDesc = "Haskell tests + accept changes to golden tests"
        , program = "cabal"
        , arguments = [ "test", "--test-option=--accept" ]
        , envVars = [] : List Laps.EnvVar
        }
    , Laps.simpleProgram
        { name = "typecheck"
        , shortDesc = "Typecheck the Dhall code"
        , program = "dhall"
        , arguments = [ "--ascii", "--file", "package.dhall" ]
        , envVars = [] : List Laps.EnvVar
        }
    , Laps.simpleScript
        { name = "format"
        , shortDesc = "Autoformat all source code"
        , interpreter = "/bin/bash"
        , contents =
            ''
            fd -e dhall --exec dhall --ascii format --inplace {}
            fd -e hs --exec stylish-haskell --inplace {}
            ''
        , envVars = [] : List Laps.EnvVar
        }
    ]
