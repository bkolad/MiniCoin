{-# LANGUAGE StandaloneDeriving #-}

module Crypto.Extended
    ( generateKeys
    , showPrivateKey
    , showPublicKey
    , mkPublicKey
    , mkPrivateKey
    , validateKeys
    , verify
    , hashWith
    , signatureToSig
    , sign
    , Account
    , Sig
    , Public
    , Private
    , Key
    , SHA256(..)
    ) where


import qualified Crypto.PubKey.ECC.ECDSA          as ECC
import qualified Crypto.PubKey.ECC.Generate       as ECC
import qualified Crypto.PubKey.ECC.Types          as ECC
import qualified Data.Attoparsec.ByteString.Char8 as B8
import qualified Data.Binary                      as B
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as Base16
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.IO                     as T
import           Data.Word8
import           Crypto.Hash --(hashWith, SHA256)
import           GHC.Generics                     (Generic)
import           Data.Aeson             (FromJSON (..), ToJSON (..), withText)
import           Data.Aeson.TH
import           Data.Semigroup         ((<>))



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


instance B.Binary (Key Public)


type Account = Key Public


instance B.Binary ECC.Point where
    put (ECC.Point x y) = do
        B.put (0 :: Word8)
        B.put x
        B.put y
    put ECC.PointO =
        B.put (1 :: Word8)

    get = do
        t <- B.get :: B.Get Word8
        case t of
            0 ->
                ECC.Point <$> B.get <*> B.get
            1 ->
                return  ECC.PointO
            _ -> error "Serialized data corrupted"

instance B.Binary ECC.PublicKey where
    put pKey = B.put $ ECC.public_q pKey
    get = ECC.PublicKey sec_t571r1 <$> B.get


instance B.Binary ECC.PrivateKey where
    put pKey = B.put $ ECC.private_d pKey
    get = ECC.PrivateKey sec_t571r1 <$> B.get


validateKeys :: (Key Public, Key Private)
             -> Either T.Text (Key Public, Key Private)
validateKeys (pub, priv) = do
    _  <- b16ToPublicKey pub
    _  <- b16ToPrivateKey priv
    return (validate pub , validate priv)
    where
        validate (K x) = K x


mkPublicKey :: B.ByteString -> Key Public
mkPublicKey = K

mkPrivateKey :: B.ByteString -> Key Private
mkPrivateKey = K

generateKeys :: IO (Key Public, Key Private)
generateKeys = do
    (pub, priv) <- ECC.generate sec_t571r1
    return  (K $ publicKeyToB16 pub, K $ privateKeyToB16 priv)

sec_t571r1 = ECC.getCurveByName ECC.SEC_t571r1


-------------------------------------------------
publicKeyToB16 :: ECC.PublicKey -> B.ByteString
publicKeyToB16 pk =  Base16.encode $ encodeToStrictBS pk

b16ToPublicKey :: Key Public -> Either T.Text ECC.PublicKey
b16ToPublicKey (K bs)= b16ToKey "Public" bs

-------------------------------------------------

privateKeyToB16 :: ECC.PrivateKey -> B.ByteString
privateKeyToB16 pk =  Base16.encode $ encodeToStrictBS pk

b16ToPrivateKey ::  Key Private -> Either T.Text ECC.PrivateKey
b16ToPrivateKey (K bs) = b16ToKey "Private" bs

-------------------------------------------------

verify :: Key Public -> Sig -> B.ByteString -> Bool
verify pk (x ,y) msg=
    let k = validKeyToByteString pk
        signature = ECC.Signature x y
    in  ECC.verify SHA256 k signature msg


sign :: Key Private -> B.ByteString -> IO ECC.Signature
sign pk =
    let k = validKeyToByteString pk
    in ECC.sign k SHA256




signatureToSig ::  ECC.Signature -> Sig
signatureToSig s = (ECC.sign_r s, ECC.sign_s s)


validKeyToByteString :: B.Binary p => Key k -> p
validKeyToByteString (K pk )=
    case pub of
        Right k -> k
        Left x -> error $ show x
    where pub =  b16ToKey "Impossible Happend, Validated key not hex encoded:" pk


encodeToStrictBS :: B.Binary b => b -> B.ByteString
encodeToStrictBS b = BL.toStrict $ B.encode b

decodeFromStrictBS :: B.Binary b => B.ByteString -> b
decodeFromStrictBS bs = B.decode $ BL.fromChunks [bs]

b16ToKey :: B.Binary b => T.Text -> B.ByteString -> Either T.Text b
b16ToKey errorMSG b16 =
    case Base16.decode b16 of
        (x, "")       -> Right $ decodeFromStrictBS x
        (_, nonEmpty) -> Left $ "ERROR:" <> errorMSG <> " Key is not HEX encoded"
