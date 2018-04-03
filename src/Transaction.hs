{-# LANGUAGE TemplateHaskell #-}

module Transaction
    ( addTransaction
    , getTransactions
    , tReqToTransaction
    , Transaction
    , TransactionRequest
    , from
    , to
    , amount
    , isTransactionSignedBySender
    ) where

import           Control.Concurrent.Extended
import qualified Crypto.Extended             as Crypto
import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              withText)
import           Data.Aeson.TH
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Serialize              as S
import           GHC.Generics                (Generic)

data TransactionRequest =
    TransactionRequest !Crypto.Account !Int
                       deriving (Eq, Generic, S.Serialize)


instance ToJSON TransactionRequest
instance FromJSON TransactionRequest


data TransactionHeader =
    TransactionHeader{ _from   :: !Crypto.Account
                     , _to     :: !Crypto.Account
                     , _amount :: !Int
                     } deriving (Eq, Generic, S.Serialize)


instance ToJSON TransactionHeader
instance FromJSON TransactionHeader


data Transaction =
     Transaction { _tHeader   :: !TransactionHeader
                 , _signature :: !Crypto.Sig
                 } deriving (Eq, Generic, S.Serialize)


instance ToJSON Transaction
instance FromJSON Transaction


tReqToTransaction :: TransactionRequest
                  -> (Crypto.Key Crypto.Public, Crypto.Key Crypto.Private)
                  -> IO Transaction
tReqToTransaction (TransactionRequest to amount) (pub, priv) = do
    sig <- signTransactionHeader priv tHeader
    return $ Transaction tHeader sig
    where tHeader =
            TransactionHeader
                    { _from    = pub
                    , _to      = to
                    , _amount  = amount
                    }

-- |
from :: Transaction -> Crypto.Account
from (Transaction header sig) = _from header

-- |
to :: Transaction -> Crypto.Account
to (Transaction header sig) = _to header

-- |
amount :: Transaction -> Int
amount (Transaction header sig) = _amount header

-- |
signature (Transaction header sig) = sig

-- |
addTransaction :: TVar [Transaction]
               -> Transaction
               -> IO()
addTransaction tQueue transaction =
   atomically $ modifyTVar' tQueue (\ls -> transaction:ls)

-- |
getTransactions :: TVar [Transaction] -> IO [Transaction]
getTransactions transQueue =
    readTVarIO transQueue

-- |
signTransactionHeader :: Crypto.Key Crypto.Private
                      -> TransactionHeader
                      -> IO Crypto.Sig
signTransactionHeader pk tr = Crypto.signatureToSig <$> Crypto.sign pk encodedTR
    where encodedTR = encodeTransactionHeader tr

-- |
encodeTransactionHeader :: TransactionHeader -> B.ByteString
encodeTransactionHeader tr = S.encode  tr

-- |
isTransactionSignedBySender :: Transaction -> Bool
isTransactionSignedBySender  tr =  Crypto.verify (from tr) (signature tr) msg
    where msg = encodeTransactionHeader (_tHeader tr)
