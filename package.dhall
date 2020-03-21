let NixEnv
    : Type
    = { srcFile : Text, attr : Optional Text, clearEnv : Bool }

let Start
    : Type
    = < Program : { program : Text, arguments : List Text }
      | Script : { interpreter : Text, contents : Text }
      >

in  { Command =
        { name : Text
        , shortDesc : Text
        , start : Start
        , nixEnv : Optional NixEnv
        , watchExtensions : List Text
        }
    , NixEnv = NixEnv
    , Start = Start
    }
