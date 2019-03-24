let ColourOutput =
      https://raw.githubusercontent.com/trskop/mainplate/master/dhall/Type/ColourOutput.dhall
      sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543

let Verbosity =
      https://raw.githubusercontent.com/trskop/verbosity/master/dhall/Verbosity/Type
      sha256:f009a39a49b1ee65651e9510778e7d72ff96820f4702a955e8f47682d72995c6

in  { verbosity = None Verbosity
    , colourOutput = None ColourOutput
    }
