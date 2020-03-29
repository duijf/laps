let Laps = ../../package.dhall

in  [ { name = "foo"
      , shortDesc = "Run the foo"
      , startOrder =
          Laps.single
            { alias = "foo"
            , executable =
                Laps.program
                  { program = "foo", arguments = [ "--bar", "--baz" ] }
            , nixEnv = None Laps.NixEnv
            , watchExtensions = [] : List Text
            }
      }
    ]
