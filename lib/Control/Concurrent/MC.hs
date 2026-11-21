module Control.Concurrent.MC (mc) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.Process

mc :: TVar Char -> TVar [(Char, String)] -> IO ()
mc qkVar kmVar = newTChanIO >>= \t -> race_ (readKey t) (runKey t)

readKey :: TChan Char -> IO ()
readKey t = loop
  where
    loop = getChar >>= \c -> case c of
      'q' -> return ()
      _   -> atomically (writeTChan t c) >> loop

runKey :: TChan Char -> IO ()
runKey t = replicateConcurrently_ 64 $ forever $
  atomically (readTChan t) >>= \c -> case c of
    'a' -> callCommand "tonwav 420 | aplay"
    'b' -> callCommand "tonwav -t 0.2 200 | aplay"
    _   -> return ()
