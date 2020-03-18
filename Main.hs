{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.List as List
import qualified Data.Foldable as Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conversions (ConvertibleStrings(..), cs)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.TH as Dhall
import           GHC.Generics (Generic)
import qualified System.IO as IO
import qualified System.Posix.Files as Files
import qualified System.Posix.Temp as Temp
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


-- Instance allowing `cs` on lists and maybes of String, Text,
-- ByteString, etc.
instance (Functor f, ConvertibleStrings a b) => ConvertibleStrings (f a) (f b) where
  convertString = fmap cs


main :: IO ()
main = do
  commands :: Set Command <- Dhall.inputFile Dhall.auto "./Laps.dhall"
  Foldable.for_ commands runCommand


getCommandProgAndArgs :: Command -> IO (String, [String], Maybe FilePath)
getCommandProgAndArgs command = do
  (prog, args, tempScript) <- case start command of
    Script{interpreter, contents} -> do
      path <- writeScript interpreter contents
      pure (path, [], Just path)

    Program{program, arguments} -> pure (cs $ program, cs $ arguments, Nothing)

  let
    watchExec = case watchExtensions command of
      [] -> []
      exts -> ["watchexec", "--exts", Foldable.fold $ List.intersperse "," $ cs $ exts, "--"]

  pure $
    case nixEnv command of
      Just (env) -> ("nix", ["run", "-f", cs $ srcFile env, "-c"] ++ watchExec ++ [prog] ++ args, tempScript)
      Nothing -> (prog, args, tempScript)


writeScript :: Text -> Text -> IO FilePath
writeScript interpreter contents = do
  (path, handle) <- Temp.mkstemp "/tmp/laps-"

  Text.hPutStr   handle "#!"
  Text.hPutStrLn handle interpreter
  Text.hPutStr   handle contents
  IO.hClose      handle

  status <- Files.getFileStatus path
  Files.setFileMode path (Files.fileMode status `Files.unionFileModes` Files.ownerExecuteMode)

  pure $ path


runCommand :: Command -> IO ()
runCommand command = do
  (prog, args, tempScript) <- getCommandProgAndArgs command
  () <$ (Process.runProcess $ Process.proc prog args)

  -- Clean up temporary script if it exists.
  maybe (pure mempty) Files.removeLink tempScript
