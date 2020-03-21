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
      ->  forall  ( Make
                  :     { nameF : Text
                        , shortDescF : Text
                        , startF : Start
                        , nixEnvF : Optional NixEnv
                        , watchExtensionsF : List Text
                        , afterF : List Command
                        }
                    ->  Command
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
      ->  \ ( Make
            :     { nameF : Text
                  , shortDescF : Text
                  , startF : Start
                  , nixEnvF : Optional NixEnv
                  , watchExtensionsF : List Text
                  , afterF : List Command
                  }
              ->  Command
            )
      ->  Make
            { nameF = args.name
            , shortDescF = args.shortDesc
            , startF = args.start
            , nixEnvF = args.nixEnv
            , watchExtensionsF = args.watchExtensions
            , afterF = [] : List Command
            }

let -- Simplest possible command so users can get started quickly.
    simple
    : { name : Text, shortDesc : Text, start : Start } -> Command
    =     \(args : { name : Text, shortDesc : Text, start : Start })
      ->  \(Command : Type)
      ->  \ ( Make
            :     { nameF : Text
                  , shortDescF : Text
                  , startF : Start
                  , nixEnvF : Optional NixEnv
                  , watchExtensionsF : List Text
                  , afterF : List Command
                  }
              ->  Command
            )
      ->  command
            (args /\ { nixEnv = None NixEnv, watchExtensions = [] : List Text })
            Command
            Make

in  { Command = { Type = Command, command = command, simple = simple }
    , NixEnv = NixEnv
    , Start = Start
    }
