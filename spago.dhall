{ name = "intl"
, dependencies =
  [ "assert"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "js-date"
  , "lists"
  , "maybe"
  , "newtype"
  , "option"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "strings"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
