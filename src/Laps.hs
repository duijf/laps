{-# LANGUAGE TemplateHaskell #-}

module Laps where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Control.Monad.Trans.Resource as Resource
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Internal as LazyByteString
import           Data.Conduit ((.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Foldable as Foldable
import           Data.Function ((&))
import qualified Data.List as List
import           Data.String.Conversions (cs)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Traversable as Traversable
import           Data.Void (Void)
import           Dhall (FromDhall)
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Src as Core
import qualified Dhall.TH as Dhall
import           GHC.IO.Exception (IOErrorType (..))
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as Posix
import qualified System.Posix.Env.ByteString as Env
import qualified System.Posix.Files as Files
import qualified System.Posix.Temp as Temp
import qualified System.Process as Process

import           OrphanInstances ()


data Program
  = Program
    { program :: Text
    , arguments :: [Text]
    } deriving (Show)

data Script
  = Script
    { interpreter :: Text
    , contents :: Text
    } deriving (Show)

data Executable = S Script | P Program
  deriving (Show)

instance FromDhall Executable where
  autoWith :: Dhall.InterpretOptions -> Dhall.Decoder Executable
  autoWith _ = Dhall.union $
    Dhall.constructor "Program"
      ( Dhall.record $ P <$>
        ( Program
          <$> Dhall.field "program" Dhall.strictText
          <*> Dhall.field "arguments" (Dhall.list Dhall.strictText)
        )
      ) <>
    Dhall.constructor "Script"
      ( Dhall.record $ S <$>
        ( Script
          <$> Dhall.field "interpreter" Dhall.strictText
          <*> Dhall.field "contents" Dhall.strictText
        )
      )


data NixEnv
  = NixEnv
    { srcFile :: Text
    , attr :: Maybe Text
    , clearEnv :: Bool
    } deriving (Show)

instance FromDhall NixEnv where
  autoWith :: Dhall.InterpretOptions -> Dhall.Decoder NixEnv
  autoWith _ = Dhall.record $
    NixEnv
      <$> Dhall.field "srcFile" Dhall.strictText
      <*> Dhall.field "attr" (Dhall.maybe Dhall.strictText)
      <*> Dhall.field "clearEnv" Dhall.bool


data Unit
  = Unit
    { executable :: Executable
    , alias :: Text
    , nixEnv :: Maybe NixEnv
    , watchExtensions :: [Text]
    } deriving (Show)

instance FromDhall Unit where
  autoWith :: Dhall.InterpretOptions -> Dhall.Decoder Unit
  autoWith opts = Dhall.record $
    Unit
      <$> Dhall.field "executable" (Dhall.autoWith opts)
      <*> Dhall.field "alias" Dhall.strictText
      <*> Dhall.field "nixEnv" (Dhall.maybe (Dhall.autoWith opts))
      <*> Dhall.field "watchExtensions" (Dhall.list Dhall.strictText)


-- We have the type parameter @a@, because we want to derive the useful
-- functor, foldable, and traversable instances. The base datatype for
-- this structure is a @Unit@, but making the type parameter generic
-- allows us to map @Units@ to IO actions and fold those together.
--
-- Later, I want to add a directed acyclic graph here as well.
data StartOrder a
  = Single a
  | Parallel [StartOrder a]
  | Serial [StartOrder a]
  | Tree a [StartOrder a]
  deriving (Show, Functor, Foldable, Traversable)

-- Decode a `StartOrder a` from the Boehm-Berarducci encoded Dhall version.
--
-- See the `StartOrder` type in `package.dhall` to see the type of this
-- expression. Here, we walk the Dhall tree for values of this type and
-- pass these to our Haskell constructors for `StartOrder`.
--
-- This code uses some Dhall internals. There is also a version of this code
-- that can be derived with Data.Fix and the `recursion-schemes` package. The
-- documentation for the `FromDhall (Fix f)` instance contains an example.
--
-- I wanted to write this myself to get an idea about what's involved in
-- rolling your own.
--
-- Benefits of writing our own instances:
--
--  - We decouple field names in the Dhall representation from field names in
--    the Haskell representation. This means we don't impose the limits of the
--    Haskell record situation on the Dhall code.
--  - Build: We do not have to depend on `recursion-schemes`.
--
-- Downsides:
--
--  - The serialization code is dense to read, boring to write, and tricky to
--    get right.
startOrderDecoder :: (FromDhall a) => Dhall.InterpretOptions -> Dhall.Decoder (StartOrder a)
startOrderDecoder opts = Dhall.Decoder extract expected
  where
    extract :: (FromDhall a) => Core.Expr Core.Src Void -> Dhall.Extractor Core.Src Void (StartOrder a)
    extract expr =
      -- Partial case expressions are safe when: the code here is correct,
      -- and Dhall's typechecker is correct. This is tricky to write and
      -- modify though, so beware.
      case expr of
        -- Get rid of the outer lambdas to get to the real structure.
        (Core.Lam _ _ (Core.Lam _ _ inner)) -> extract inner

        -- Cases for Single, Paralle, and Serial.
        (Core.App field@(Core.Field _ _) value) ->
          case (field, value) of
            (Core.Field _ "single", _) ->
              -- Leaf of the recursive structure. Dispatch to `FromDhall a`.
              Single <$> Dhall.extract (Dhall.autoWith opts) value

            (Core.Field _ "parallel", Core.ListLit _ list) ->
              Parallel <$> traverse extract (Foldable.toList list)

            (Core.Field _ "serial", Core.ListLit _ list) ->
              Serial <$> traverse extract (Foldable.toList list)

            _ -> Dhall.typeError expected field

        -- Because `App` is left associative in the AST, the AST function is
        -- 'inside out' for `Unit -> List StartOrder -> StartOrder`. It has
        -- this structure:
        --
        -- App (App `treeField` `recordLit`) `nestedStartOrder`
        --                      ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^
        --                       Unit         List StartOrder
        (Core.App (Core.App (Core.Field _ "tree") (value)) (Core.ListLit _ list)) ->
          Tree <$> Dhall.extract (Dhall.autoWith opts) value
               <*> traverse extract (Foldable.toList list)

        -- If our code is correct, this branch should never match. You can
        -- add pTraceShowId to help with debugging.
        actual -> Dhall.typeError expected actual

    expected :: Core.Expr Core.Src Void
    expected = $(Dhall.staticDhallExpression "(./package.dhall).StartOrder")

instance (FromDhall a) => FromDhall (StartOrder a) where
  autoWith opts = startOrderDecoder opts


data Command
  = Command
  { name :: Text
  , shortDesc :: Text
  , startOrder :: StartOrder Unit
  } deriving (Show)

instance FromDhall Command where
  autoWith _opts = Dhall.record $
    Command
      <$> Dhall.field "name" Dhall.strictText
      <*> Dhall.field "shortDesc" Dhall.strictText
      <*> Dhall.field "startOrder" Dhall.auto


main :: IO ()
main = do
  commands :: [Command] <- List.sortOn name <$> Dhall.input Dhall.auto "./Laps.dhall"

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
      -- Does not do what we want yet: we don't have a way to
      -- stop the computation in case a single process returns
      -- errors.
      void $ forStartOrder runUnit (startOrder command)
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


getProcessConfig :: (MonadIO m, MonadResource m) => Unit -> m (Process.CreateProcess, IO.Handle)
getProcessConfig unit = do
  (prog, args) <- case executable unit of
    (S Script{interpreter, contents}) -> do
      path <- writeScript interpreter contents
      pure (path, [])

    (P Program{program, arguments}) -> pure (cs $ program, cs $ arguments)

  let
    watchExec = case watchExtensions unit of
      [] -> []
      exts -> ["watchexec", "--exts", Foldable.fold $ List.intersperse "," $ cs $ exts, "--"]

  (termPrimFd, termSecFd) <- liftIO $ Posix.openPseudoTerminal

  hRead <- liftIO $ Posix.fdToHandle termPrimFd
  hWrite <- liftIO $ Posix.fdToHandle termSecFd

  _ <- Resource.register $ IO.hClose hRead
  _ <- Resource.register $ IO.hClose hWrite

  let outputStream = Process.UseHandle hWrite
      setOutputStream cp = cp { Process.std_out = outputStream, Process.std_err = outputStream }

  pure $
    case nixEnv unit of
      Just (env) -> (setOutputStream $ Process.proc "nix" (["run", "-f", cs $ srcFile env, "-c"] ++ watchExec ++ [prog] ++ args), hRead)
      Nothing -> (setOutputStream $ Process.proc prog args, hRead)


writeScript :: (MonadIO m, MonadResource m) => Text -> Text -> m FilePath
writeScript interpreter contents = do
  path <- liftIO $ do
    (path, handle) <- Temp.mkstemp "/tmp/laps-"

    Text.hPutStr   handle "#!"
    Text.hPutStrLn handle interpreter
    Text.hPutStr   handle contents
    IO.hClose      handle

    makeExecutable path
    pure $ path

  void $ Resource.register $ Files.removeLink path
  pure path


makeExecutable :: FilePath -> IO ()
makeExecutable path = do
  status <- Files.getFileStatus path
  let newMode = Files.fileMode status `Files.unionFileModes` Files.ownerExecuteMode
  Files.setFileMode path newMode


-- @Traversable.for@ specialized to @StartOrder@ and @IO@. Evaluates the action in parallel
-- for all @StartOrder.Parallel@ entries and the list of children of @StartOrder.Tree@.
forStartOrder :: (a -> IO b) -> StartOrder a -> IO (StartOrder b)
forStartOrder f startOrder = case startOrder of
  Single a -> Single <$> f a
  Serial as -> Serial <$> Traversable.for as (forStartOrder f)
  Parallel as -> Parallel <$> Async.forConcurrently as (forStartOrder f)
  Tree a as -> do
    a' <- f a
    as' <- Async.forConcurrently as (forStartOrder f)
    pure $ Tree a' as'


runUnit :: Unit -> IO ()
runUnit unit = Resource.runResourceT $ do
  (createProc, readHandle) <- getProcessConfig unit
  liftIO $ Process.withCreateProcess createProc $ \_stdin _stdout _stderr _proc -> do
    let aliasPretty =
          (ANSI.setSGRCode [ANSI.Reset, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan])
          <> cs (alias unit) <> ": " <> ANSI.setSGRCode [ANSI.Reset]
    Conduit.runConduit $
      sourceHandleCatchIoErr readHandle
        .| Conduit.linesUnboundedAscii
        .| Conduit.map (\str -> cs aliasPretty <> str)
        .| Conduit.mapM_ ByteString.putStrLn

    pure ()


-- @Conduit.sourceHandle@ which catches @GHC.IO.Exception.IOErrorType.HardwareFault@s.
-- These correspond to @errno 5@ on Linux which is returned when you attempt to read from
-- a pseudo terminal which has been closed because the child process has exited (but I
-- might be doing something very wrong here).
sourceHandleCatchIoErr :: MonadIO m => IO.Handle -> Conduit.ConduitT i ByteString m ()
sourceHandleCatchIoErr h = loop
  where
    loop = do
      result <- liftIO $ Exception.tryJust catchHardwareFaults (ByteString.hGetSome h LazyByteString.defaultChunkSize)
      case result of
        Right bs | ByteString.null bs -> pure ()
        Right bs | otherwise -> Conduit.yield bs >> loop
        Left _ -> pure ()


catchHardwareFaults :: IOError -> Maybe IOError
catchHardwareFaults ioe =
  if isHardwareFault $ IO.ioeGetErrorType ioe
  then Just ioe
  else Nothing


isHardwareFault :: IOErrorType -> Bool
isHardwareFault = \case
  HardwareFault -> True
  _ -> False
