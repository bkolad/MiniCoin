{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified BlockChain             as BCN
import           Control.Concurrent.STM
import           Logger                 as Log
import           Options.Applicative
import qualified Server
import           System.Environment
import           Types
import qualified Data.Text              as T


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
main = do
    Args miner  <- execParser opts
    queue <-  newTVarIO []
    blockChain <-  BCN.initBlockChain
    let env = Env (State blockChain queue) Log.createLogger (Account <$> miner)
    Server.startApp env
