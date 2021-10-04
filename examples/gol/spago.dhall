let config = ../../spago.dhall

in    config
    ⫽ { sources = config.sources # [ "examples/gol/**/*.purs" ]
      , dependencies = config.dependencies
      }
