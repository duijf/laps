module Main where

import qualified Data.List as List
import qualified Data.Foldable as Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Dhall
import           Dhall (FromDhall)
import           GHC.Generics (Generic)
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as Process


data NixEnv
  = NixEnv
  { nixSrcFile :: FilePath
  } deriving (Eq, Generic, Ord, Show)

instance FromDhall NixEnv


data Command
  = Command
    { name :: String
    , shortDesc :: String
    , program :: String
    , arguments :: [String]
    , nixEnv :: Maybe NixEnv
    }
  deriving (Eq, Generic, Ord, Show)

instance FromDhall Command


data Watch
  = Watch
    { command :: Command
    , extensions :: [String]
    }
  deriving (Eq, Generic, Ord, Show)

instance FromDhall Watch


data Unit = C Command | W Watch
  deriving (Eq, Generic, Ord, Show)

instance FromDhall Unit


main :: IO ()
main = do
  let
    build :: Command = Command
      { name = "build"
      , shortDesc = "Build the project"
      , program = "cabal"
      , arguments = ["new-build"]
      , nixEnv = Just (NixEnv { nixSrcFile = "default.nix" })
      }
    units :: Set Unit = Set.fromList
      [ C build
      , W Watch
        { command = build
        , extensions = ["hs", "cabal"]
        }
      ]

  Foldable.for_ units runUnit


getCommandProgAndArgs :: Command -> (String, [String])
getCommandProgAndArgs command =
  case nixEnv command of
    Just (env) -> ("nix", ["run", "-f", nixSrcFile env, "-c"] ++ [prog] ++ args)
    Nothing -> (prog, args)
  where
    args = arguments command
    prog = program command


getWatchProgAndArgs :: Watch -> (String, [String])
getWatchProgAndArgs watch = ("nix", ["run", "-c", "watchexec"] ++ extFilter ++ ["--"] ++ [prog] ++ args)
  where
    extFilter = case extensions watch of
      [] -> []
      exts -> ["--exts", Foldable.fold $ List.intersperse "," exts]
    (prog, args) = getCommandProgAndArgs (command watch)


runUnit :: Unit -> IO ()
runUnit unit = case unit of
  C command -> do
    let (prog, args) = getCommandProgAndArgs command
    startProc prog args

  W watch -> do
    let (prog, args) = getWatchProgAndArgs watch
    startProc prog args
  where
    startProc prog args = () <$ (Process.runProcess $ Process.proc prog args)
