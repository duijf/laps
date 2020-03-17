let NixEnv
    : Type
    = { srcFile : Text, attr : Optional Text, pure : Bool }

in  { Command =
        { name : Text
        , shortDesc : Text
        , program : Text
        , arguments : List Text
        , nixEnv : Optional NixEnv
        , watchExtensions : List Text
        }
    , NixEnv = NixEnv
    }
