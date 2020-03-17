{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as List
import qualified Data.Foldable as Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conversions (cs)
import           Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.TH as Dhall
import           GHC.Generics (Generic)
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as Process


Dhall.makeHaskellTypes
  [ Dhall.SingleConstructor
    { Dhall.typeName = "NixEnv"
    , Dhall.constructorName = "NixEnv"
    , Dhall.code  = "(./Types.dhall).NixEnv"
    }
  , Dhall.MultipleConstructors
    { Dhall.typeName = "Start"
    , Dhall.code  = "(./Types.dhall).Start"
    }
  , Dhall.SingleConstructor
    { Dhall.typeName = "Command"
    , Dhall.constructorName = "Command"
    , Dhall.code = "(./Types.dhall).Command"
    }
  ]


deriving instance Show Start
deriving instance Eq Start
deriving instance Ord Start
deriving instance Generic Start
instance FromDhall Start


deriving instance Show NixEnv
deriving instance Eq NixEnv
deriving instance Ord NixEnv
deriving instance Generic NixEnv
instance FromDhall NixEnv


deriving instance Show Command
deriving instance Eq Command
deriving instance Ord Command
deriving instance Generic Command
instance FromDhall Command


main :: IO ()
main = do
  commands :: Set Command <- Dhall.inputFile Dhall.auto "./Laps.dhall"
  Foldable.for_ commands runCommand


getCommandProgAndArgs :: Command -> (String, [String])
getCommandProgAndArgs command =
  case nixEnv command of
    Just (env) -> ("nix", ["run", "-f", cs $ srcFile env, "-c"] ++ watchExec ++ [prog] ++ args)
    Nothing -> (prog, args)
  where
    watchExec = case watchExtensions command of
      [] -> []
      exts -> ["watchexec", "--exts", Foldable.fold $ List.intersperse "," $ cs <$> exts, "--"]
    args = cs <$> arguments command
    prog = cs $ program command


runCommand :: Command -> IO ()
runCommand command = do
  let (prog, args) = getCommandProgAndArgs command
  () <$ (Process.runProcess $ Process.proc prog args)
