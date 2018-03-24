{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


module Types
    ( Transaction(..)
    , TransactionRequest(..)
    , Block(..)
    , BlockData(..)
    , BlockChain(..)
    , State(..)
    , HASH(..)
    , SHA256(..)
    , Genesis(..)
    , Account(..)
    , Env(..)
    , Key (..)
    , Sig (..)
    , Public
    , Private
    , TransactionHeader(..)
    , showPrivateKey
    , showPublicKey
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
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           GHC.Generics           (Generic)


data Public
data Private

newtype Key k = K B.ByteString
type Sig = (Integer, Integer)


deriving instance Eq (Key Public)
deriving instance Show (Key Public)
deriving instance Ord (Key Public)
deriving instance Generic (Key Public)


showPublicKey :: Key Public -> T.Text
showPublicKey (K pubKey) = T.decodeLatin1 pubKey

showPrivateKey :: Key Private -> T.Text
showPrivateKey (K privKey) = T.decodeLatin1 privKey




instance ToJSON (Key Public) where
    toJSON (K p)= toJSON (T.decodeLatin1 p)


instance FromJSON (Key Public) where
    parseJSON = withText "PublicKey" $ pure . K . T.encodeUtf8

instance Binary (Key Public)


type Account = Key Public

data TransactionRequest =
    TransactionRequest{ to     :: !Account
                      , amount :: !Int
                      } deriving (Eq, Show, Generic)


$(deriveJSON defaultOptions ''TransactionRequest)


data TransactionHeader =
    TransactionHeader{ _from   :: !Account
                     , _to     :: !Account
                     , _amount :: !Int
                     } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''TransactionHeader)


instance Binary TransactionHeader

data Transaction =
     Transaction { _tHeader   :: !TransactionHeader
                 , _signature :: !Sig
                 } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Transaction)

instance Binary Transaction



data Block = Block
            { _minerAccount :: !Account
            , _minerReward  :: !Int
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

type Genesis = Block


type BlockChain = (Genesis, [BlockData])


data State = State
           { _blockChain          :: TVar BlockChain
           , _pendingTransactions :: TVar [Transaction]
           }


data Env = Env
         { _state   :: !State
         , _log     :: !(T.Text -> IO ())
         , _isMiner :: !Bool
         , _keys    :: !(Key Public, Key Private)
         }
