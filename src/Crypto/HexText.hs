module Crypto.HexText
    where
{--

import           Data.Aeson              (FromJSON (..), ToJSON (..), withText)
import qualified Data.Binary             as B
import qualified Data.ByteArray          as E
import qualified Data.ByteArray.Encoding as E
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy         as LB

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           GHC.Generics            (Generic)

import           Crypto.Hash


newtype HexText = HexText T.Text
    deriving (Eq, Show, Generic)


instance ToJSON HexText where
    toJSON (HexText h)= toJSON h


instance FromJSON HexText where
    parseJSON = withText "HexText" $ pure . HexText

instance B.Binary HexText


byteStringToHexText :: B.ByteString -> HexText
byteStringToHexText bs = HexText $ convertToBase16 $ hashWith SHA256 bs
    where
        convertToBase16 !hash = T.decodeLatin1 $ E.convertToBase E.Base16 hash


lByteStringToHexText :: LB.ByteString -> HexText
lBbyteStringToHexText bs = HexText $ convertToBase16 $ hashWith SHA256 $ LB.fromChunks bs
    where
        convertToBase16 !hash = T.decodeLatin1 $ E.convertToBase E.Base16 hash

        -}
