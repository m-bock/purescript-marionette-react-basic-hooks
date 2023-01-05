{ name = "purescript-marionette-react-basic-hooks"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "effect"
  , "exceptions"
  , "marionette"
  , "maybe"
  , "newtype"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
