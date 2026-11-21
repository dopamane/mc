module Control.Concurrent.MC (mc) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

mc
  :: Eq a
  => Int               -- ^ concurrency factor
  -> TVar a            -- ^ quit value
  -> TVar [(a, IO ())] -- ^ IO map
  -> IO a              -- ^ input value
  -> IO ()
mc c q m i = newTChanIO >>= \t -> race_ (reader q i t) (runner c m t)

reader :: Eq a => TVar a -> IO a -> TChan a -> IO ()
reader qv i t = loop
  where
    loop = i >>= \a -> readTVarIO qv >>= \q ->
      when (a /= q) $ atomically (writeTChan t a) >> loop

runner :: Eq a => Int -> TVar [(a, IO ())] -> TChan a -> IO ()
runner c m t = replicateConcurrently_ c $ forever $ join $ atomically $
  fmap (fromMaybe $ return ()) $ lookup <$> readTChan t <*> readTVar m
