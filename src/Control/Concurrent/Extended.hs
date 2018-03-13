module  Control.Concurrent.Extended
    (module Control.Concurrent.STM
    ,module Control.Concurrent
    ,tryWriteTBQueue
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad


tryWriteTBQueue :: TBQueue a -> a -> IO ()
tryWriteTBQueue tbq a = atomically $ do
    isTBQFull <- isFullTBQueue tbq
    unless isTBQFull $ writeTBQueue tbq a
