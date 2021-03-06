{ name = "grid-reactors"
, license = "MIT"
, repository = "https://github.com/Eugleo/purescript-grid-reactors.git"
, dependencies =
  [ "arrays"
  , "canvas-action"
  , "colors"
  , "css"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "st"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
