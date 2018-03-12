{-# LANGUAGE OverloadedStrings #-}


module Logger
    (createLogger
    ,logM
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import           Types


createLogger :: T.Text -> IO()
createLogger = print

logM :: MonadIO m => T.Text -> ReaderT Env m ()
logM txt = do
    fl <- _log <$> ask
    liftIO $ fl txt
