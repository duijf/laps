module Main where

import Control.Concurrent
import Control.Concurrent.STM.TBMChan
import Conduit
import Data.Conduit.TMChan
import Data.Conduit.Process.Typed


main :: IO ()
main = do
  chan <- newTBMChanIO 1000

  _ <- forkIO $ do
    let
      procSpec = setStdout createSource
               $ proc "/bin/echo" ["Hello world"]

    withProcess procSpec $ \proc -> do
      runConduit $ do
        (getStdout proc) .| sinkTBMChan chan

  print "hi"

  runConduit $
    sourceTBMChan chan .| mapM_C print

  pure ()
