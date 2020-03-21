let NixEnv
    : Type
    = { srcFile : Text, attr : Optional Text, clearEnv : Bool }

let Start
    : Type
    = < Program : { program : Text, arguments : List Text }
      | Script : { interpreter : Text, contents : Text }
      >

let -- We want users to be able to specify a tree of commands to execute.
    -- Trees are a recursive datatype. However, Dhall does not allow this
    -- definition:
    --
    -- ```
    -- let Tree : Type = { n : Natural, children : Tree } in Tree`
    -- ```
    --
    -- So we resort to some fancy things. This is called a Church encoding.
    Command
    : Type
    =     forall (Command : Type)
      ->  forall  ( command
                  : { construct :
                            { name : Text
                            , shortDesc : Text
                            , start : Start
                            , nixEnv : Optional NixEnv
                            , watchExtensions : List Text
                            , after : List Command
                            }
                        ->  Command
                    }
                  )
      ->  Command

let command
    :     { name : Text
          , shortDesc : Text
          , start : Start
          , nixEnv : Optional NixEnv
          , watchExtensions : List Text
          }
      ->  Command
    =     \ ( args
            : { name : Text
              , shortDesc : Text
              , start : Start
              , nixEnv : Optional NixEnv
              , watchExtensions : List Text
              }
            )
      ->  \(Command : Type)
      ->  \ ( command
            : { construct :
                      { name : Text
                      , shortDesc : Text
                      , start : Start
                      , nixEnv : Optional NixEnv
                      , watchExtensions : List Text
                      , after : List Command
                      }
                  ->  Command
              }
            )
      ->  command.construct
            { name = args.name
            , shortDesc = args.shortDesc
            , start = args.start
            , nixEnv = args.nixEnv
            , watchExtensions = args.watchExtensions
            , after = [] : List Command
            }

let -- Simplest possible command so users can get started quickly.
    simple
    : { name : Text, shortDesc : Text, start : Start } -> Command
    =     \(args : { name : Text, shortDesc : Text, start : Start })
      ->  \(Command : Type)
      ->  \ ( command
            : { construct :
                      { name : Text
                      , shortDesc : Text
                      , start : Start
                      , nixEnv : Optional NixEnv
                      , watchExtensions : List Text
                      , after : List Command
                      }
                  ->  Command
              }
            )
      ->  command@1
            (args /\ { nixEnv = None NixEnv, watchExtensions = [] : List Text })
            Command
            command

in  { Command.Type = Command
    , Command.command = command
    , Command.simple = simple
    , NixEnv = NixEnv
    , Start = Start
    }
