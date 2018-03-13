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

newtype Args = Args
  { minerAcc :: Maybe T.Text
  } deriving Show


args :: Parser Args
args = Args
    <$> optional (strOption
         ( long "minerAccount"
         <> help "Miner Account"))

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc
      <> progDesc "Lambda Coin node")


-- ./BlockChain-exe --minerAccount ""
main :: IO ()
main = Log.withStdoutLogging $ do
    env <- prepareEnv
    Server.startApp env


prepareEnv = do
    Args miner  <- execParser opts
    initState <- initialState
    return $ Env initState mkLogger (Account <$> miner)
    where
        mkLogger txt =  do
            concurrentLogger <- Log.newTBQLogger 1000
            Log.mkLogger concurrentLogger txt

        initialState = State <$> BCN.initBlockChain
                             <*> newTVarIO []
