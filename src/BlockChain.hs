{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE  RecordWildCards  #-}

module BlockChain
    ( mine
    , getBlockChain
    , initBlockChain
    ) where

import           Control.Concurrent.Extended
import qualified Crypto.Extended             as Crypto
import qualified Data.Binary                 as B
import qualified Data.ByteArray.Encoding     as E
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as BL
import qualified Data.List.NonEmpty          as NEL
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import qualified Transaction                 as TCN
import           Types
import           Data.List (foldl')



initBlockChain :: IO (TVar BlockChain)
initBlockChain =
    newTVarIO (genesis, [])

hardcodedMinerForGensis = Crypto.mkPublicKey "00010100000000000000487c1741d7169bd8ac5ca752a28f736b231f901857b56c4d1a38bbf6302a1224a039fb13bc50f9f0b61ac03d825067904d7ffdb0e40f8d4f5bc68df3c64b6a3b4c2407035a53c3cc0601010000000000000048d16539e35721426503171e245cf049390d1c675097340d7ae35431209a32c36b7370a25c5b772947b0232b893302e62ce3d8911add4b0dad283a4478408418ebf231dc7896c4ac06"

genesis :: Genesis
genesis = Block { _minerAccount = hardcodedMinerForGensis
                , _minerReward = 0
                , _transactions  = []
                , _nonce         = 99
                }


mineBlock :: Account -> BlockChain -> [Transaction] -> Int -> BlockData
mineBlock minerAcc blockChain transactions nonce =
    let block = Block
              { _minerAccount = minerAcc
              , _minerReward = 50
              , _transactions = validTransactions blockChain transactions
              , _nonce = nonce
              }
        in BlockData block (hash256BC blockChain)



validTransactions :: BlockChain -> [Transaction] -> [Transaction]
validTransactions  blockChain ts =
    snd $ foldTransactions (balanceMap blockChain) ts


arrow :: (a -> b, a -> c) -> a -> (b, c)
arrow (f, g) = \a -> (f a, g a)


balanceMap :: BlockChain -> M.Map Account Int
balanceMap (genesis, bc) =
    let allBlocks = genesis : (_block <$> bc)

        allMinerRewards =  arrow ( _minerAccount, _minerReward) <$> allBlocks
        rewardBalanceMap = foldl' addMinerReward M.empty allMinerRewards

        allTransactions =  allBlocks >>= _transactions
        transactionBalanceMap = foldl' addTransaction rewardBalanceMap allTransactions

    in transactionBalanceMap
        where
            addMinerReward !acc (miner, amount) =
                  M.insertWith (+) miner amount acc


addTransaction ::  M.Map Account Int -> Transaction -> M.Map Account Int
addTransaction !balanceMap tr  =
    let _from    = TCN.from tr
        _to      = TCN.to tr
        _amount  = TCN.amount tr

        m = M.insertWith (+) _from (- _amount) balanceMap
    in  M.insertWith (+) _to _amount m


foldTransactions :: M.Map Account Int
                 -> [Transaction]
                 -> (M.Map Account Int ,[Transaction])
foldTransactions balanceMap pendingTransactions =
     foldl' updateBalanceMap (balanceMap, []) pendingTransactions
     where
         updateBalanceMap !(balanceMap, ts) tr =
             if isTransactionValid balanceMap tr then
                 (addTransaction balanceMap tr, tr : ts )
                 else (balanceMap, ts)



isTransactionValid :: M.Map Account Int -> Transaction -> Bool
isTransactionValid balanceMap transaction  =
    case M.lookup (TCN.from transaction) balanceMap of
         Nothing      -> False
         Just balance ->
            balance >= TCN.amount transaction
                    && TCN.isTransactionSignedBySender transaction



validateChain :: BlockChain -> Bool
validateChain (_, []) = True
validateChain (g, x:xs) =
     _parentHash x == hashOfTheTail
                  && validateChain blockChainTail
    where
        blockChainTail = (g, xs)
        hashOfTheTail = hash256BC blockChainTail


verifyProofOfWork :: BlockData -> Bool
verifyProofOfWork bd = B.isPrefixOf difficulty hash
    where
        HASH hash = hash256 bd
        difficulty = "000"


mine :: Account -> BlockChain -> [Transaction] -> BlockChain
mine miner blockChain tcns =
       let blockData = head -- it safe here, we are filtering stream
                $ filter verifyProofOfWork
                $ (\nonce -> mineBlock miner blockChain tcns nonce) <$> [1 ..]
       in append blockChain blockData


getBlockChain :: TVar BlockChain -> IO BlockChain
getBlockChain blockChain = readTVarIO blockChain


hash256BC :: BlockChain -> HASH
hash256BC !bc = case bc of
    (g,[])    -> hash256 g
    (_, x:xs) -> hash256 x


hash256 b  =
    let !serialized = BL.toStrict $ B.encode b
        convertTo !hash = E.convertToBase E.Base16 hash
    in HASH $ convertTo $ Crypto.hashWith Crypto.SHA256 serialized


append :: BlockChain -> BlockData -> BlockChain
append (g, xs) !x = (g, x:xs)
