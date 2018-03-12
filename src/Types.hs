{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Types
    ( Transaction(..)
    , Block(..)
    , BlockData(..)
    , BlockChain(..)
    , State(..)
    , HASH(..)
    , SHA256(..)
    , Genesis(..)
    , Account(..)
    , Env(..)
    , module Data.Semigroup
    ) where

import           Control.Concurrent.STM
import           Crypto.Hash
import           Data.Aeson             (FromJSON (..), ToJSON (..), withText)
import           Data.Aeson.TH
import           Data.Binary
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List.NonEmpty     as NEL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           GHC.Generics           (Generic)
import           Data.Semigroup         ((<>))


data Account = Account !T.Text | Faucet
    deriving (Eq, Show, Generic, Ord)

$(deriveJSON defaultOptions ''Account)

instance Binary Account


data Transaction =
     Transaction { from   :: !Account
                 , to     :: !Account
                 , amount :: !Int
                 } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Transaction)

instance Binary Transaction


data Block = Block
            { _minerAccount :: !Account
            , _transactions :: ![Transaction]
            , _nonce        :: !Int
            }
            deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Block)
instance Binary Block


newtype HASH = HASH B.ByteString
    deriving (Eq, Show, Generic)


instance ToJSON HASH where
    toJSON (HASH h)= toJSON (T.decodeLatin1 h)


instance FromJSON HASH where
    parseJSON = withText "HASH" $ pure . HASH . T.encodeUtf8


instance Binary HASH


data BlockData = BlockData
               { _block      :: !Block
               , _parentHash :: !HASH
               } deriving (Eq, Show, Generic)


$(deriveJSON defaultOptions ''BlockData)
instance Binary BlockData

newtype Genesis = Genesis Transaction
        deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Genesis)
instance Binary Genesis


type BlockChain = (Genesis, [BlockData])


data State = State
           { _blockChain          :: TVar BlockChain
           , _pendingTransactions :: TVar [Transaction]
           }


data Env = Env
         { _state    :: !State
         , _log      :: !(T.Text -> IO ())
         , _minerAcc :: !(Maybe Account)
         }
