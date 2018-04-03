{-# LANGUAGE BangPatterns #-}

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
    , HASH
    , HexBS (..)
    , hash256
    ) where


import           Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA          as ECC
import qualified Crypto.PubKey.ECC.Generate       as ECC
import qualified Crypto.PubKey.ECC.Types          as ECC
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   withText)
import           Data.Aeson.TH
import qualified Data.Attoparsec.ByteString.Char8 as B8
import qualified Data.ByteArray.Encoding          as E
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as Base16
import           Data.Either.Combinators          (mapLeft)
import           Data.Semigroup                   ((<>))
import qualified Data.Serialize                   as S
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.IO                     as T
import           Data.Word8
import           GHC.Generics                     (Generic)


newtype HexBS = HexBS B.ByteString
    deriving (Eq, Ord, Generic, S.Serialize)


instance ToJSON HexBS where
    toJSON hBS = toJSON $ hexBSToText hBS


instance FromJSON HexBS where
    parseJSON = withText "HexBS" $ pure . textToHexBS

type HASH = HexBS

hexBSToText :: HexBS -> T.Text
hexBSToText (HexBS bs) = T.decodeLatin1 bs

textToHexBS  =  HexBS . T.encodeUtf8

-- |
hash256 :: S.Serialize p => p -> HASH
hash256 b  =
    let !serialized = S.encode b
        convertTo !hash = E.convertToBase E.Base16 hash
    in HexBS $ convertTo $ hashWith SHA256 serialized


data Public
data Private

newtype Key k = K HexBS
type Sig = (Integer, Integer)


deriving instance Eq (Key Public)
deriving instance Ord (Key Public)
deriving instance Generic (Key Public)
deriving instance S.Serialize (Key Public)


showPublicKey :: Key Public -> T.Text
showPublicKey (K pubKey) = hexBSToText pubKey

showPrivateKey :: Key Private -> T.Text
showPrivateKey (K privKey) = hexBSToText privKey


instance ToJSON (Key Public) where
    toJSON pk = toJSON $ showPublicKey pk


instance FromJSON (Key Public) where
    parseJSON = withText "PublicKey" $ pure . K . textToHexBS


type Account = Key Public


instance S.Serialize ECC.Point where
    put (ECC.Point x y) = do
        S.put (0 :: Word8)
        S.put x
        S.put y
    put ECC.PointO =
        S.put (1 :: Word8)

    get = do
        t <- S.get :: S.Get Word8
        case t of
            0 ->
                ECC.Point <$> S.get <*> S.get
            1 ->
                return  ECC.PointO
            _ -> error "Serialized data corrupted"


instance S.Serialize ECC.PublicKey where
    put pKey = S.put $ ECC.public_q pKey
    get = ECC.PublicKey sec_t571r1 <$> S.get


instance S.Serialize ECC.PrivateKey where
    put pKey = S.put $ ECC.private_d pKey
    get = ECC.PrivateKey sec_t571r1 <$> S.get


validateKeys :: (Key Public, Key Private)
             -> Either T.Text (Key Public, Key Private)
validateKeys (pub, priv) = do
    _  <- b16ToPublicKey pub
    _  <- b16ToPrivateKey priv
    return (validate pub , validate priv)
    where
        validate (K x) = K x


mkPublicKey :: HexBS -> Key Public
mkPublicKey = K

mkPrivateKey :: HexBS -> Key Private
mkPrivateKey = K

generateKeys :: IO (Key Public, Key Private)
generateKeys = do
    (pub, priv) <- ECC.generate sec_t571r1
    return  ( mkPublicKey $ publicKeyToB16 pub
            , mkPrivateKey $ privateKeyToB16 priv)

sec_t571r1 = ECC.getCurveByName ECC.SEC_t571r1


-------------------------------------------------
publicKeyToB16 :: ECC.PublicKey -> HexBS
publicKeyToB16 =  HexBS . Base16.encode . encodeToStrictBS

b16ToPublicKey :: Key Public -> Either T.Text ECC.PublicKey
b16ToPublicKey (K bs)= b16ToKey "Public" bs

-------------------------------------------------

privateKeyToB16 :: ECC.PrivateKey -> HexBS
privateKeyToB16  =  HexBS . Base16.encode . encodeToStrictBS

b16ToPrivateKey ::  Key Private -> Either T.Text ECC.PrivateKey
b16ToPrivateKey (K bs) = b16ToKey "Private" bs

-------------------------------------------------

verify :: Key Public -> Sig -> B.ByteString -> Bool
verify pk (x ,y) msg =
    let k = validKeyToByteString pk
        signature = ECC.Signature x y
    in  ECC.verify SHA256 k signature msg


sign :: Key Private
     -> B.ByteString
     -> IO ECC.Signature
sign pk =
    let k = validKeyToByteString pk
    in ECC.sign k SHA256


signatureToSig ::  ECC.Signature -> Sig
signatureToSig s = (ECC.sign_r s, ECC.sign_s s)


validKeyToByteString :: S.Serialize p => Key k -> p
validKeyToByteString (K pk)=
    case pub of
        Right k -> k
        Left x  -> error $ show x
    where pub =  b16ToKey "Impossible Happend, Validated key not hex encoded:" pk


encodeToStrictBS ::  S.Serialize b => b -> B.ByteString
encodeToStrictBS b =  S.encode b


decodeFromStrictBS ::  S.Serialize b => B.ByteString -> Either String b
decodeFromStrictBS bs = S.decode bs


b16ToKey ::  S.Serialize b => T.Text
         -> HexBS
         -> Either T.Text b
b16ToKey errorMSG (HexBS b16) =
    case Base16.decode b16 of
        (x, "")       -> mapLeft T.pack $ decodeFromStrictBS x
        (_, nonEmpty) -> Left $ "ERROR:" <> errorMSG <> " Key is not HEX encoded"
