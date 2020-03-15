module Main where

import qualified Data.Foldable as Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as Process


data NixEnv
  = NixEnv
  { nixSrcFile :: FilePath
  } deriving (Eq, Ord, Show)


data Unit
  = Command
  { name :: String
  , shortDesc :: String
  , program :: String
  , arguments :: [String]
  , nixEnv :: Maybe NixEnv
  } deriving (Eq, Ord, Show)


main :: IO ()
main = do
  let
    units :: Set Unit = Set.fromList
      [ Command
        { name = "build"
        , shortDesc = "Build the project"
        , program = "cabal"
        , arguments = ["new-build"]
        , nixEnv = Just (NixEnv { nixSrcFile = "default.nix" })
        }
      ]

  runUnits units


getProcessConfig :: Unit -> ProcessConfig () () ()
getProcessConfig unit = case nixEnv unit of
  Just (env) ->
    let
      args = ["run", "-f", nixSrcFile env, "-c"] ++ [program unit] ++ arguments unit
    in
      Process.proc "nix" args
  Nothing -> Process.proc (program unit) (arguments unit)


runUnits :: Set Unit -> IO ()
runUnits units = Foldable.for_ units (startProc . getProcessConfig)
  where
    -- TODO: Check exit codes in some way. Report results.
    startProc config = () <$ Process.runProcess config
