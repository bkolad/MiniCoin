module Types
    ( TCN.Transaction
    , TCN.TransactionRequest
    , Block(..)
    , BlockData(..)
    , BlockChain(..)
    , State(..)
    , HASH(..)
    , Genesis(..)
    , Account(..)
    , Env(..)
    , Key (..)
    , Sig (..)
    , Public
    , Private
    , showPrivateKey
    , showPublicKey
    , module Data.Semigroup
    ) where

import           Control.Concurrent.STM
import           Crypto.Extended
import           Data.Aeson             (FromJSON (..), ToJSON (..), withText)
import           Data.Aeson.TH
import           Data.Binary
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.List.NonEmpty     as NEL
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Transaction as TCN
import           GHC.Generics           (Generic)



data Block = Block
            { _minerAccount :: !Account
            , _minerReward  :: !Int
            , _transactions :: ![TCN.Transaction]
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
           , _pendingTransactions :: TVar [TCN.Transaction]
           }


data Env = Env
         { _state   :: !State
         , _log     :: !(T.Text -> IO ())
         , _isMiner :: !Bool
         , _keys    :: !(Key Public, Key Private)
         }
