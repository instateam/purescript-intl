{ name = "intl"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "generics-rep"
  , "js-date"
  , "maybe"
  , "newtype"
  , "nullable"
  , "option"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "strings"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
