let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

let NixEnv
    : Type
    = { srcFile : Text, attr : Optional Text, clearEnv : Bool }

let Executable
    : Type
    = < Program : { program : Text, arguments : List Text }
      | Script : { interpreter : Text, contents : Text }
      >

let Unit
    : Type
    = { executable : Executable
      , alias : Text
      , nixEnv : Optional NixEnv
      , watchExtensions : List Text
      }

let -- We want users to be able to specify a tree of commands to execute.
    -- Trees are a recursive datatype. However, Dhall does not allow this
    -- definition:
    --
    -- ```
    -- let Tree : Type = { n : Natural, children : Tree } in Tree`
    -- ```
    --
    -- So we resort to some fancy things. This is called a Boehm-Berarducci
    -- encoding. This is a typed variant of Church encoding.
    --
    -- Gabriel has a nice blogpost explaining how this works in a Dhall's
    -- precursors: http://www.haskellforall.com/2016/04/data-is-code.html
    StartOrder
    : Type
    =     forall (StartOrder : Type)
      ->  forall  ( startOrder
                  : { single : Unit -> StartOrder
                    , parallel : List StartOrder -> StartOrder
                    , serial : List StartOrder -> StartOrder
                    , tree : Unit -> List StartOrder -> StartOrder
                    }
                  )
      ->  StartOrder

let Command
    : Type
    = { name : Text, shortDesc : Text, startOrder : StartOrder }

let single
    : Unit -> StartOrder
    =     \(unit : Unit)
      ->  \(StartOrder : Type)
      ->  \ ( startOrder
            : { single : Unit -> StartOrder
              , parallel : List StartOrder -> StartOrder
              , serial : List StartOrder -> StartOrder
              , tree : Unit -> List StartOrder -> StartOrder
              }
            )
      ->  startOrder.single unit

let parallel
    : List StartOrder -> StartOrder
    =     \(nested : List StartOrder)
      ->  \(StartOrder : Type)
      ->  \ ( startOrder
            : { single : Unit -> StartOrder
              , parallel : List StartOrder -> StartOrder
              , serial : List StartOrder -> StartOrder
              , tree : Unit -> List StartOrder -> StartOrder
              }
            )
      ->  startOrder.parallel
            ( Prelude.List.map
                StartOrder@1
                StartOrder
                (\(so : StartOrder@1) -> so StartOrder startOrder)
                nested
            )

let serial
    : List StartOrder -> StartOrder
    =     \(nested : List StartOrder)
      ->  \(StartOrder : Type)
      ->  \ ( startOrder
            : { single : Unit -> StartOrder
              , parallel : List StartOrder -> StartOrder
              , serial : List StartOrder -> StartOrder
              , tree : Unit -> List StartOrder -> StartOrder
              }
            )
      ->  startOrder.serial
            ( Prelude.List.map
                StartOrder@1
                StartOrder
                (\(so : StartOrder@1) -> so StartOrder startOrder)
                nested
            )

let tree
    : Unit -> List StartOrder -> StartOrder
    =     \(unit : Unit)
      ->  \(nested : List StartOrder)
      ->  \(StartOrder : Type)
      ->  \ ( startOrder
            : { single : Unit -> StartOrder
              , parallel : List StartOrder -> StartOrder
              , serial : List StartOrder -> StartOrder
              , tree : Unit -> List StartOrder -> StartOrder
              }
            )
      ->  startOrder.tree
            unit
            ( Prelude.List.map
                StartOrder@1
                StartOrder
                (\(so : StartOrder@1) -> so StartOrder startOrder)
                nested
            )

in  { Command = Command
    , NixEnv = NixEnv
    , Executable = Executable
    , Unit = Unit
    , StartOrder = StartOrder
    , program = Executable.Program
    , script = Executable.Script
    , single = single
    , serial = serial
    , parallel = parallel
    }
