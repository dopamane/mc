module Control.Concurrent.MC (mc) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

mc
  :: Eq a
  => Int               -- ^ concurrency factor
  -> Maybe a           -- ^ quit value
  -> (a -> IO ())      -- ^ unknown value
  -> TVar [(a, IO ())] -- ^ IO map
  -> IO a              -- ^ input value
  -> IO ()
mc c q u m i = newTChanIO >>= \t -> race_ (reader q i t) $
  replicateConcurrently_ c $ forever $ join $ atomically $
  (\a l -> fromMaybe (u a) $ lookup a l) <$> readTChan t <*> readTVar m

reader :: Eq a => Maybe a -> IO a -> TChan a -> IO ()
reader q i t = loop
  where
    loop = i >>= \a -> when (Just a /= q) $ atomically (writeTChan t a) >> loop
