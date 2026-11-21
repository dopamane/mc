module Control.Concurrent.MC (mc) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

mc :: Int -> TVar Char -> TVar [(Char, IO ())] -> IO ()
mc cnt qVar mVar = newTChanIO >>= \t ->
  race_ (reader qVar t) (runner cnt mVar t)

reader :: TVar Char -> TChan Char -> IO ()
reader qVar t = loop
  where
    loop = getChar >>= \c -> readTVarIO qVar >>= \q ->
      when (c /= q) $ atomically (writeTChan t c) >> loop

runner :: Int -> TVar [(Char, IO ())] -> TChan Char -> IO ()
runner cnt mVar t = replicateConcurrently_ cnt $ forever $ join $ atomically $
  fmap (fromMaybe $ return ()) $ lookup <$> readTChan t <*> readTVar mVar
