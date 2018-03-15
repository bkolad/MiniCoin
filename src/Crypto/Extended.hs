{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.Extended
    (
    ) where


import qualified Crypto.PubKey.ECC.ECDSA    as ECC
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types    as ECC
import qualified Data.Binary                as B
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import           Data.Word8
import           GHC.Generics               (Generic)
import           Types



newtype B16 = B16 B.ByteString

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


generateKeys :: IO (ECC.PublicKey, ECC.PrivateKey)
generateKeys = ECC.generate sec_t571r1

sec_t571r1 = ECC.getCurveByName ECC.SEC_t571r1


-------------------------------------------------
publicKeyToB16 :: ECC.PublicKey -> B16
publicKeyToB16 pk = B16 $ Base16.encode $ encodeToStrictBS pk

b16ToPublicKey :: B16 -> Either T.Text ECC.PublicKey
b16ToPublicKey = b16ToKey

-------------------------------------------------

privateKeyToB16 :: ECC.PrivateKey -> B16
privateKeyToB16 pk = B16 $ Base16.encode $ encodeToStrictBS pk

b16ToPrivateKey :: B16 -> Either T.Text ECC.PrivateKey
b16ToPrivateKey  = b16ToKey

-------------------------------------------------

verify :: B16 -> ECC.Signature -> B.ByteString -> Bool
verify pkB16 sig msg=
    let pub = b16ToPublicKey pkB16
    in undefined --ECC.verify SHA256 pub sig msg

sign :: ECC.PrivateKey -> B.ByteString -> IO ECC.Signature
sign pk = ECC.sign pk SHA256

encodeToStrictBS :: B.Binary b => b -> B.ByteString
encodeToStrictBS b = BL.toStrict $ B.encode b

decodeFromStrictBS :: B.Binary b => B.ByteString -> b
decodeFromStrictBS bs = B.decode $ BL.fromChunks [bs]

b16ToKey :: B.Binary b => B16 -> Either T.Text b
b16ToKey (B16 b16) =
    case Base16.decode b16 of
        (x, "")       -> Right $ decodeFromStrictBS x
        (_, nonEmpty) -> Left "ERROR: Key is not HEX encoded"
