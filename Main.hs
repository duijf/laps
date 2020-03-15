module Main where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as Process


data NixEnv
  = NixEnv
  { nixSrcFile :: FilePath
  } deriving (Eq, Ord, Show)


data Command
  = Command
    { name :: String
    , shortDesc :: String
    , program :: String
    , arguments :: [String]
    , nixEnv :: Maybe NixEnv
    }
  deriving (Eq, Ord, Show)


data Watch
  = Watch
    { command :: Command
    , extensions :: Maybe (NonEmpty String)
    }
  deriving (Eq, Ord, Show)


data Unit = C Command | W Watch
  deriving (Eq, Ord, Show)


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
        , extensions = NonEmpty.nonEmpty ["hs", "cabal"]
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
      Just exts -> ["--exts", Foldable.fold $ NonEmpty.intersperse "," exts]
      Nothing -> []
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
