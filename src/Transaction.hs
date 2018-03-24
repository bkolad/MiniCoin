{-# LANGUAGE TemplateHaskell #-}

module Transaction
    ( addTransaction
    , getTransactions
    , tReqToTransaction
    ) where

import           Control.Concurrent.Extended
import           Types
import qualified Crypto.Extended as Crypto


tReqToTransaction :: TransactionRequest
                  -> (Key Public, Key Private)
                  -> IO Transaction
tReqToTransaction tr (pub, priv) = do
    sig <- Crypto.signTransactionHeader priv tHeader
    return $ Transaction
                { _tHeader = tHeader
                , _signature = sig
                }
    where tHeader =
            TransactionHeader
                    { _from    = pub
                    , _to      = to tr
                    , _amount  = amount tr
                    }


addTransaction :: TVar [Transaction] -> Transaction -> IO()
addTransaction tQueue transaction =
   atomically $ modifyTVar' tQueue (\ls -> transaction:ls)


getTransactions :: TVar [Transaction] -> IO [Transaction]
getTransactions transQueue =
    readTVarIO transQueue
