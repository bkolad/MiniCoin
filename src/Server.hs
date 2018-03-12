{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
    ( startApp
    , app
    ) where

import qualified BlockChain                 as BCN
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                  as T
import qualified Logger                     as Log
import           Network.Wai
import           Network.Wai.Handler.Warp   as Warp
import           Servant
import           Servant.Server
import qualified Transaction                as TCN
import           Types


type API = "transactions":> "new"
                :> ReqBody '[JSON] Transaction
                :> Post '[JSON] NoContent
        :<|> "transactions":> "pending"
                :> Get '[JSON] [Transaction]
        :<|> "blockchain"
                :> Get '[JSON] BlockChain
        :<|> "blockchain":>"mine"
                :> Get '[JSON] NoContent


startApp :: Env -> IO ()
startApp env = 
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
    State bc pending <- askForSate
    Log.logM "start mining"
    (x,y) <-liftIO $ readState bc pending
    let !newBCN =  BCN.mine (Account "M1") x y
    Log.logM $ "block mined: " <> T.pack (show newBCN)
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


--http://localhost:8080/blockchain
chain :: ReaderT Env Handler BlockChain
chain = do
        blockchain <- _blockChain <$> askForSate
        liftIO $ BCN.getBlockChain blockchain

registerNode = undefined
resolve = undefined


--curl -X POST -d '{"from":{"tag":"Account","contents":"John"},"to":{"tag":"Account","contents":"Me"},"amount":99}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/transactions/new/
newTransaction :: Transaction -> ReaderT Env Handler NoContent
newTransaction transaction = do
      pendTransactions <- _pendingTransactions <$> askForSate
      liftIO $ TCN.addTransaction pendTransactions transaction
      return NoContent

--http://localhost:8080/transactions/pending
pendingTransactions :: ReaderT Env Handler [Transaction]
pendingTransactions = do
    State _ pendTransactions <- askForSate
    liftIO $ TCN.getTransactions pendTransactions


askForSate :: ReaderT Env Handler State
askForSate = do
    env <- ask :: ReaderT Env Handler Env
    return $ _state env
