{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "argonaut"
    , "argonaut-codecs"
    , "argonaut-core"
    , "argonaut-generic"
    , "console"
    , "effect"
    , "generics-rep"
    , "numbers"
    , "ordered-collections"
    , "parsing"
    , "psci-support"
    , "spec"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
