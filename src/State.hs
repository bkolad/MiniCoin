module State
    ( State(..)
    , Env(..)
    ) where

import           Control.Concurrent.STM
import           Crypto.Extended
import qualified Data.Text              as T
import qualified Transaction as TCN
import qualified BlockChain as BCN
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)


data State = State
           { _blockChain          :: TVar BCN.BlockChain
           , _pendingTransactions :: TVar [TCN.Transaction]
           }


data Env = Env
         { _state   :: !State
         , _log     :: !(T.Text -> IO ())
         , _isMiner :: !Bool
         , _keys    :: !(Key Public, Key Private)
         }
