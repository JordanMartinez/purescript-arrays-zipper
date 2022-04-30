{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "gen"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/JordanMartinez/purescript-arrays-zipper"
}
