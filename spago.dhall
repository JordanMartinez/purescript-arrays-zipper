{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "spec"
  , "control"
  , "aff"
  , "effect"
  , "foldable-traversable"
  , "gen"
  , "maybe"
  , "partial"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/JordanMartinez/purescript-arrays-zipper"
}
