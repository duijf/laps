let Laps = ../../package.dhall

let mkUnit
    : Text -> Laps.Unit
    =     \(name : Text)
      ->  { alias = name
          , executable =
              Laps.program { program = "foo", arguments = [ "--bar", "--baz" ] }
          , nixEnv = None Laps.NixEnv
          , watchExtensions = [] : List Text
          }

let mkSingle
    : Text -> Laps.StartOrder
    = \(name : Text) -> Laps.single (mkUnit name)

in  [ { name = "foo"
      , shortDesc = "foo"
      , startOrder =
          Laps.tree
            (mkUnit "foo")
            [ mkSingle "bar"
            , Laps.parallel [ mkSingle "baz", mkSingle "quix" ]
            , Laps.serial
                [ mkSingle "1"
                , mkSingle "2"
                , Laps.tree (mkUnit "3") [ mkSingle "4" ]
                ]
            ]
      }
    ]
