module Main where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List as List
import           Data.String.Conversions (cs)
import qualified Dhall
import qualified GHC.IO.Encoding
import qualified System.FilePath as FilePath
import qualified System.IO
import           Test.QuickCheck.Instances ()
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Test.Tasty.QuickCheck as QC
import qualified Text.Pretty.Simple as PP

import qualified Laps


-- For each `.dhall` file in the `tests/deserialization` directory:
--
--  1. Try deserializing it into a command.
--  2. Pretty print it using the show instance.
--  3. Convert to a bytestring.
--
-- The testing framework takes care of diffing the bytestring against
-- a reference value stored in the file with the `.golden` extension.
getDeserializationTests :: IO Tasty.TestTree
getDeserializationTests = do
  dhallFiles <- Golden.findByExtension [".dhall"] "tests/deserialization"

  let
    deserialize :: FilePath -> IO LByteString.ByteString
    deserialize filePath = do
      commands :: [Laps.Command] <- List.sortOn Laps.name <$> Dhall.inputFile Dhall.auto filePath
      pure $ cs $ PP.pShowNoColor commands

  pure $ Tasty.testGroup "Deserialization"
    [ Golden.goldenVsStringDiff
        dhallFile
        (\reference new -> ["diff", "-u", reference, new])
        outputFile
        (deserialize dhallFile)
    | dhallFile <- dhallFiles
    , let outputFile = FilePath.replaceExtension dhallFile ".golden"
    ]


stringProcessing :: Tasty.TestTree
stringProcessing = Tasty.testGroup "Unit tests"
  [ QC.testProperty "renderClearlines strips all content before ANSI clear code" $
      \bytestring -> (Laps.renderClearLines (ByteString.concat [bytestring, Laps.ansiClear, "asdf"]) == "asdf")
  ]


main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

  deserializationTests <- getDeserializationTests

  let
    testTree =
      Tasty.testGroup "Laps Tests"
        [ deserializationTests
        , stringProcessing
        ]

  Tasty.defaultMain testTree
