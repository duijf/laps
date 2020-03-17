let Types = ./Types.dhall

in  let build =
          { name = "build"
          , shortDesc = "Build the project"
          , program = "cabal"
          , arguments = [ "new-build" ]
          , nixEnv = Some { nixSrcFile = "default.nix" }
          }

    in  [ Types.Unit.C build
        , Types.Unit.W { command = build, extensions = [ "hs", "cabal" ] }
        ]
