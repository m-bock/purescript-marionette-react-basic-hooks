let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221229/packages.dhall
        sha256:a6af1091425f806ec0da34934bb6c0ab0ac1598620bbcbb60a7d463354e7d87c

in  upstream

  with marionette =
      { dependencies =
        [ "aff"
        , "arrays"
        , "console"
        , "datetime"
        , "effect"
        , "either"
        , "enums"
        , "foldable-traversable"
        , "maybe"
        , "newtype"
        , "node-readline"
        , "now"
        , "ordered-collections"
        , "prelude"
        , "refs"
        , "strings"
        , "transformers"
        , "tuples"
        ]
      , repo =
          "https://github.com/thought2/purescript-marionette.git"
      , version =
          "v1.0.0"
      }