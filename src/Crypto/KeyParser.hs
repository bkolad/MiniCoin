
module Crypto.KeyParser
    ( parseFromFile
    ) where

import qualified Crypto.Extended                  as Crypto
import qualified Data.Attoparsec.ByteString.Char8 as B8
import qualified Data.ByteString                  as B
import qualified Data.Text                        as T
import           Types


parsePublicKey :: B8.Parser (Key Public)
parsePublicKey =
    Crypto.mkPublicKey <$> (pubKeyStr *> pubKey)
    where
        pubKeyStr = B8.string "PUBLIC Key: "
        pubKey = B8.takeTill isEOL


parsePrivateKey :: B8.Parser (Key Private)
parsePrivateKey =
    Crypto.mkPrivateKey <$> (privKeyStr *> privKey)
    where
        privKeyStr = B8.string "PRIVATE Key: "
        privKey = B8.takeTill isEOL


parseKeys :: B8.Parser (Key Public, Key Private)
parseKeys = do
    pubKey <- parsePublicKey
    B8.skipWhile isEOL
    privKey <- parsePrivateKey
    return (pubKey, privKey)

isEOL c = c == '\n' || c == '\r'


parseFromFile :: FilePath
              -> IO (Either T.Text (Key Public, Key Private))
parseFromFile dir = do
    content <- B.readFile dir
    case B8.parse parseKeys content of
            B8.Done i keys -> return $ Crypto.validateKeys keys
            other -> return $ Left $ "Corrupted file: " <> T.pack(show dir)
