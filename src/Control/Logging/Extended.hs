{-# LANGUAGE OverloadedStrings #-}


module Control.Logging.Extended
    (logM
    ,Log.withStdoutLogging
    ,concurrentLog
    ,newTBQLogger
    ,Logger(..)
    ,mkLogger
    ) where

import           Control.Concurrent.Extended
import qualified Control.Logging             as Log
import           Control.Monad               (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text                   as T
import           Prelude                     hiding (log)
import           Types


data Logger = Simple
            | Concurrent (TBQueue T.Text)


mkLogger :: Logger -> T.Text -> IO ()
mkLogger Simple txt = do
    tid <- myThreadId
    Log.log $ T.pack(show tid) <>" "<>txt
mkLogger (Concurrent tbq) txt = tryWriteTBQueue tbq txt


concurrentLog :: TBQueue T.Text -> T.Text -> IO()
concurrentLog tbq text = tryWriteTBQueue tbq text


logM :: MonadIO m => T.Text -> ReaderT Env m ()
logM txt = do
    fl <- _log <$> ask
    liftIO $ fl txt


newTBQLogger :: Int -> IO (Logger)
newTBQLogger n = do
    tbq <- newTBQueueIO n
    forkIO $ forever $ do
        txt <- atomically $ readTBQueue tbq
        Log.log $ txt
    return $ Concurrent tbq
