module Control.Concurrent.MC (mc) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

mc :: Int -> TVar Char -> TVar [(Char, IO ())] -> IO ()
mc c q m = newTChanIO >>= \t -> race_ (reader q t) (runner c m t)

reader :: TVar Char -> TChan Char -> IO ()
reader qv t = loop
  where
    loop = getChar >>= \c -> readTVarIO qv >>= \q ->
      when (c /= q) $ atomically (writeTChan t c) >> loop

runner :: Int -> TVar [(Char, IO ())] -> TChan Char -> IO ()
runner c m t = replicateConcurrently_ c $ forever $ join $ atomically $
  fmap (fromMaybe $ return ()) $ lookup <$> readTChan t <*> readTVar m
