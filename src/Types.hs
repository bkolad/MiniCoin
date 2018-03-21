{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


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
    , Key (..)
    , Public
    , Private
    , ValidPublic
    , ValidPrivate
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
data ValidPublic
data ValidPrivate

newtype Key k = K B.ByteString

deriving instance Show (Key ValidPrivate)


deriving instance Eq (Key ValidPublic)
deriving instance Show (Key ValidPublic)
deriving instance Ord (Key ValidPublic)
deriving instance Generic (Key ValidPublic)

instance Binary (Key ValidPublic)

showPublicKey :: Key ValidPublic -> T.Text
showPublicKey (K pubKey) = T.decodeLatin1 pubKey

showPrivateKey :: Key ValidPrivate -> T.Text
showPrivateKey (K privKey) = T.decodeLatin1 privKey

instance ToJSON (Key ValidPublic) where
    toJSON = toJSON . showPublicKey


instance FromJSON (Key ValidPublic) where
    parseJSON = withText "PublicKey" $ pure . K . T.encodeUtf8


data Account = Account !(Key ValidPublic) | Faucet
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

type Genesis = Block


type BlockChain = (Genesis, [BlockData])


data State = State
           { _blockChain          :: TVar BlockChain
           , _pendingTransactions :: TVar [Transaction]
           }


data Env = Env
         { _state   :: !State
         , _log     :: !(T.Text -> IO ())
         , _isMiner :: Bool
         , _keys    :: !(Key ValidPublic, Key ValidPrivate)
         }
