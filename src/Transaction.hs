{-# LANGUAGE TemplateHaskell #-}

module Transaction
    ( addTransaction
    , getTransactions
    ) where

import           Control.Concurrent.STM
import           Types


addTransaction :: TVar [Transaction] -> Transaction -> IO()
addTransaction tQueue transaction =
   atomically $ modifyTVar' tQueue (\ls -> transaction:ls)


getTransactions :: TVar [Transaction] -> IO [Transaction]
getTransactions transQueue =
    readTVarIO transQueue
