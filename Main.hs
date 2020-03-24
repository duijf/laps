{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad (when)
import           Data.Fix (Fix (..))
import qualified Data.Fix as Fix
import qualified Data.Foldable as Foldable
import           Data.Function ((&))
import           Data.Functor.Foldable (embed)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String.Conversions (ConvertibleStrings (..), cs)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Dhall (FromDhall, ToDhall)
import qualified Dhall
import qualified Dhall.TH as Dhall
import           GHC.Generics (Generic)
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Posix.Env.ByteString as Env
import qualified System.Posix.Files as Files
import qualified System.Posix.Temp as Temp
import           System.Process.Typed (ProcessConfig)
import qualified System.Process.Typed as Process


data Executable
  = Program
    { program :: Text
    , arguments :: [Text]
    }
  | Script
    { interpreter :: Text
    , contents :: Text
    }


data NixEnv
  = NixEnv
    { srcFile :: Text
    , attr :: Maybe Text
    , clearEnv :: Bool
    }


data Unit
  = Unit
    { executable :: Executable
    , alias :: Text
    , nixEnv :: Maybe NixEnv
    , watchExtensions :: [Text]
    }


-- Should we add a directed acyclic graph?
data StartOrder a
  = Single a
  | Parallel [StartOrder a]
  | Serial [StartOrder a]
  | Tree Unit [StartOrder a]
  deriving (Functor, Foldable, Traversable)


data Command
  = Command
  { name :: Text
  , shortDesc :: Text
  , startOrder :: StartOrder Unit
  }


-- Instance allowing `cs` on lists and maybes of String, Text,
-- ByteString, etc.
instance (Functor f, ConvertibleStrings a b) => ConvertibleStrings (f a) (f b) where
  convertString = fmap cs


main :: IO ()
main = do
  -- commands :: [Command] <- List.sortOn name <$> Dhall.input Dhall.auto "./Laps.dhall"
  commands :: [Command] <- pure []

  args :: [Text] <- cs <$> Env.getArgs

  when (length args == 0) (do
    printHelp commands
    Exit.exitSuccess)

  when (length args > 1) (do
    printColor ANSI.Red "error:"
    Text.putStr " Laps expects a single argument "
    printBold   "COMMAND"
    Text.putStr " got "
    printBold   (Text.intercalate " " args)
    Text.putStr "\n"
    printColor ANSI.Cyan " hint:"
    Text.putStr " Run "
    printBold   "laps"
    Text.putStr " without arguments for a list of available "
    printBold   "COMMAND"
    Text.putStr "s\n"
    Exit.exitFailure)

  List.find (\c -> name c == head args) commands & \case
    Just command ->
      -- Does not do what we want yet: this just runs everything
      -- in series while we wait for an actual implementation.
      Foldable.traverse_ runUnit (startOrder command)
    Nothing -> do
      printColor ANSI.Red "error:"
      Text.putStr " No command "
      printBold   (head args)
      Text.putStr " defined in Laps.dhall.\n"
      printColor ANSI.Cyan " hint:"
      Text.putStr " Available commands are: "
      Foldable.fold $ List.intersperse (Text.putStr ", ") $ printBold <$> name <$> commands
      Text.putStr "\n"


printColor :: ANSI.Color -> Text -> IO ()
printColor color t = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
  Text.putStr t
  ANSI.setSGR [ANSI.Reset]


printBold :: Text -> IO ()
printBold t = do
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  Text.putStr t
  ANSI.setSGR [ANSI.Reset]


printHelp :: [Command] -> IO ()
printHelp commands = do
  Text.putStrLn "Laps - Project automation\n"
  Text.putStr "Define commands in "
  printBold   "Laps.dhall"
  Text.putStr ". Use "
  printBold   "laps COMMAND"
  Text.putStr " to run them.\n\n"
  Text.putStr "COMMANDS\n"
  Foldable.for_ commands printCommand


printCommand :: Command -> IO ()
printCommand command = do
  Text.putStrLn ""
  Text.putStr   "  "
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  Text.putStrLn (name command)
  ANSI.setSGR [ANSI.Reset]
  Text.putStr   "    "
  Text.putStrLn (shortDesc command)


getCommandProgAndArgs :: Unit -> IO (String, [String], Maybe FilePath)
getCommandProgAndArgs unit = do
  (prog, args, tempScript) <- case executable unit of
    Script{interpreter, contents} -> do
      path <- writeScript interpreter contents
      pure (path, [], Just path)

    Program{program, arguments} -> pure (cs $ program, cs $ arguments, Nothing)

  let
    watchExec = case watchExtensions unit of
      [] -> []
      exts -> ["watchexec", "--exts", Foldable.fold $ List.intersperse "," $ cs $ exts, "--"]

  pure $
    case nixEnv unit of
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


runUnit :: Unit -> IO ()
runUnit unit = do
  (prog, args, tempScript) <- getCommandProgAndArgs unit
  () <$ (Process.runProcess $ Process.proc prog args)

  -- Clean up temporary script if it exists.
  maybe (pure mempty) Files.removeLink tempScript
