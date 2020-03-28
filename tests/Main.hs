module Main where

import qualified Test.Tasty as Tasty


getGoldenTests :: IO Tasty.TestTree
getGoldenTests = pure $ Tasty.testGroup "Golden tests" mempty


main :: IO ()
main = do
  goldenTests <- getGoldenTests

  let
    testTree =
      Tasty.testGroup "Laps Tests"
        [ goldenTests
        ]

  Tasty.defaultMain testTree
