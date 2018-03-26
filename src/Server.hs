{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( startApp
    , app
    ) where

import qualified BlockChain                  as BCN
import           Control.Concurrent
import           Control.Concurrent.Extended
import qualified Control.Logging.Extended    as Log
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                   as T
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp
import           Servant
import           Servant.Server
import qualified Transaction                 as TCN
import           Types


type API = "transactions":> "new"
                :> ReqBody '[JSON] TransactionRequest
                :> Post '[JSON] NoContent
        :<|> "transactions":> "pending"
                :> Get '[JSON] [Transaction]
        :<|> "blockchain"
                :> Get '[JSON] BlockChain
        :<|> "blockchain":> "mine"
                :> Get '[JSON] NoContent


startApp :: Env -> IO ()
startApp env =  do
    _log env "Starting server"
    Warp.run 8080 $ app env


nt :: Env -> ReaderT Env Handler a -> Handler a
nt env r = runReaderT r env


app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server

api :: Proxy API
api = Proxy


server :: ServerT API (ReaderT Env Handler)
server = newTransaction
       :<|> pendingTransactions
       :<|> chain
       :<|> mineBlock


mineBlock :: ReaderT Env Handler NoContent
mineBlock = do
    isMiner <- _isMiner <$> ask
    if isMiner then
        startMining
    else do
        Log.logM  "Node is not a miner"
        return NoContent


startMining :: ReaderT Env Handler NoContent
startMining = do
    State bc pending <- askForState
    Log.logM  "start mining "
    (bChain, pTrans) <-liftIO $ readState bc pending
    (pubKey, _)<-_keys <$> ask
    -- Making sure mining is done in the current thread
    let !newBCN =  BCN.mine pubKey bChain pTrans
    Log.logM  "block mined "
    _ <-liftIO $ atomically $ do
        writeTVar bc newBCN
        writeTVar pending []
    return NoContent
    where
        readState bc pending =
            atomically $
                do b <- readTVar bc
                   p <- readTVar pending
                   return (b, p)


chain :: ReaderT Env Handler BlockChain
chain = do
        blockchain <- _blockChain <$> askForState
        liftIO $ BCN.getBlockChain blockchain

newTransaction :: TransactionRequest -> ReaderT Env Handler NoContent
newTransaction transactionRequest  = do
      keys <- _keys <$> ask
      transaction <- liftIO $ TCN.tReqToTransaction transactionRequest keys
      pendTransactions <- _pendingTransactions <$> askForState
      liftIO $ TCN.addTransaction pendTransactions transaction
      return NoContent


--http://localhost:8080/transactions/pending
pendingTransactions :: ReaderT Env Handler [Transaction]
pendingTransactions = do
    State _ pendTransactions <- askForState
    liftIO $ TCN.getTransactions pendTransactions


askForState :: ReaderT Env Handler State
askForState = do
    env <- ask :: ReaderT Env Handler Env
    return $ _state env
