{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BlockChain                  as BCN
import           Control.Applicative
import           Control.Concurrent.Extended
import           Control.Logging.Extended    as Log
import qualified Data.Text                   as T
import           Options.Applicative
import qualified Server
import           System.Environment
import           Types
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding     as T

import qualified Crypto.Extended as Crypto
import qualified Data.ByteString        as B
import qualified Crypto.KeyParser as KP
import qualified Crypto.Extended as Crypto


data Args = RunNode Bool FilePath
          | GenAccount FilePath
  deriving Show

defaultPath = "keysDir/k1.txt"

args :: Parser Args
args = runNode <|> genAccount
    where
        runNode = RunNode
            <$> option auto (
                  long "isMiner"
                 <> help "Is node a miner"
                 <> value False
                 <> showDefault)
            <*> strOption
                 ( long "dirPath"
                  <> help "Path to keys directory"
                  <> value defaultPath
                  <> showDefault)

        genAccount = GenAccount
                <$> strOption
                     ( long "genAccount"
                     <> help ("Generate Account, "
                            <>"provide path to store keys files ")
                     <> value defaultPath
                     <> showDefault)

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc
      <> progDesc "Lambda Coin node")


-- ./BlockChain-exe --minerAccount ""
main :: IO ()
main = Log.withStdoutLogging runProgram

runProgram = do
    opts  <- execParser opts
    let logger txt = Log.mkLogger Log.simple  txt
    case opts of
        RunNode isMiner dir -> do
            logger "Starting node with settings:"
            logger $"isMiner: " <> T.pack (show isMiner)

            eKey <- KP.parseFromFile dir
            case eKey of
                Left err -> do
                    logger "Can't process Key file"
                    logger err
                    error "Node Exit, invalid keys"

                Right (pub, priv) -> do
                    logger  "Running node for Account: "
                    logger $ Crypto.showPublicKey pub

                    initState <- initialState
                    concLogger <- Log.newTBQLogger
                    Server.startApp
                        Env { _state = initState
                            , _log =  Log.mkLogger concLogger
                            , _isMiner =  isMiner
                            , _keys = (pub, priv)
                            }
                    where
                        initialState = State <$> BCN.initBlockChain
                                             <*> newTVarIO []


        GenAccount fp -> do
                (pub, priv) <- Crypto.generateKeys
                let content = fileContent pub priv
                saveKeysToFile fp content
                logger $ "keys saved to " <> T.pack fp
            where

                fileContent pub priv =  T.encodeUtf8 $
                                "PUBLIC Key: "
                                <> showPublicKey pub
                                <> "\n"
                                <> "PRIVATE Key: "
                                <> showPrivateKey priv


saveKeysToFile ::  FilePath -> B.ByteString -> IO ()
saveKeysToFile fp t = B.writeFile fp t
