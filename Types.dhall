let NixEnv = { nixSrcFile : Text } : Type

in  { Unit =
        < C :
            { name : Text
            , shortDesc : Text
            , program : Text
            , arguments : List Text
            , nixEnv : Optional NixEnv
            }
        -- This doesn't work: I haven't found a way to refer to C from
        -- this constructor yet. (I guess that doesn't help with Turing
        -- completeness). Look into [1] to address or change the data to
        -- avoid recursion.
        -- [1]: https://github.com/dhall-lang/dhall-lang/wiki/How-to-translate-recursive-code-to-Dhall
        | W : { command : C, extensions : List Text }
        >
    }
