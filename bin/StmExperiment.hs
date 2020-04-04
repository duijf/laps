import           Conduit
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMChan
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8 as C8
import           Data.Conduit.TMChan
import           System.IO
import           System.Process


main :: IO ()
main = do
  chan <- newTBMChanIO 1000

  _ <- forkIO $ do
    runResourceT $ do
      (pRead, pWrite) <- liftIO createPipe
      _ <- register $ hClose pRead
      _ <- register $ hClose pWrite
      _ <- register $ atomically $ closeTBMChan chan
      let
        createProc
          = CreateProcess
            { cmdspec = ShellCommand ">&2 /bin/echo stderr; /bin/echo stdout; >&2 /bin/echo stderr2; echo stdout2; echo stdout3"
            , cwd = Nothing
            , env = Nothing
            , std_in = NoStream
            , std_out = UseHandle pWrite
            , std_err = UseHandle pWrite
            , close_fds = False
            , create_group = False
            , delegate_ctlc = False
            , detach_console = False
            , create_new_console = False
            , new_session = False
            , child_group = Nothing
            , child_user = Nothing
            , use_process_jobs = False
            }
        procOutput = sourceHandle pRead

      -- Stdin, stdout, and stderr are all Nothing because we passed
      -- NoStream and Handle respectively.
      liftIO $ withCreateProcess createProc $ \_stdin _stdout _stderr -> do
        pure $ runConduit $ do
          procOutput .| sinkTBMChan chan

  runConduit $ do
    sourceTBMChan chan
    .| concatMapC (split '\n')
    .| filterC (/= "")
    .| mapC ("command | " <>)
    .| mapM_C C8.putStrLn

  pure ()
