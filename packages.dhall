let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210622/packages.dhall sha256:c4949646febb03c7b1f329c9f48921c3a1e6afee133330fd24b5aa4a88112973

in  upstream
  with option =
    { dependencies =
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "codec"
      , "codec-argonaut"
      , "datetime"
      , "either"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "lists"
      , "maybe"
      , "psci-support"
      , "profunctor"
      , "prelude"
      , "record"
      , "simple-json"
      , "spec"
      , "transformers"
      , "tuples"
      , "type-equality"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/joneshf/purescript-option.git"
    , version = "v9.0.0"
    }
