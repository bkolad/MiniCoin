{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module BlockChain
    ( mine
    , getBlockChain
    , registerNode
    , resolve
    , initBlockChain
    ) where

import           Control.Concurrent.Extended
import qualified Crypto.Hash             as H
import qualified Data.Binary             as B
import qualified Data.ByteArray.Encoding as E
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as BL
import qualified Data.List.NonEmpty      as NEL
import qualified Data.Map.Strict         as M
import qualified Data.Text               as T
import qualified Transaction             as TCN
import           Types


initBlockChain :: IO (TVar BlockChain)
initBlockChain =
    newTVarIO (genesis, [])



minerReward miner = Transaction Faucet miner 12

genesis = Genesis $ Transaction Faucet (Account "John") 1000

mineBlock :: Account -> BlockChain -> [Transaction] -> Int -> BlockData
mineBlock minerAcc blockChain transactions nonce =
    let block = Block
              { _minerAccount = minerAcc
              , _transactions = minerReward minerAcc : (validTransactions blockChain transactions)
              , _nonce = nonce
              }
        in BlockData block (hash256BC blockChain)

validTransactions :: BlockChain -> [Transaction] -> [Transaction]
validTransactions blockChain ts =
    filter isTransactionValid ts
     where
        balances = balanceMap $ allTransactions blockChain
        -- order does not matter
        allTransactions (Genesis t, bc) = t : (_block <$> bc >>= _transactions)
        -- this implementation allows double spending
        isTransactionValid (Transaction from _  amount) =
            case M.lookup from balances of
                 Nothing      -> False
                 Just balance -> balance >= amount

        balanceMap ts = foldl mkMap M.empty ts
            where
                mkMap acc (Transaction from to amount) =
                    let  m = M.insertWith (+) from (-amount) acc
                    in M.insertWith (+) to amount m


validateChain :: BlockChain -> Bool
validateChain (_, []) = True
validateChain (g, x:xs) =
     (_parentHash x == hashOfTheTail) && (validateChain blockChainTail)
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
    in HASH $ convertTo $ H.hashWith SHA256 serialized


append :: BlockChain -> BlockData -> BlockChain
append (g, xs) !x = (g, x:xs)


registerNode = undefined
resolve = undefined
