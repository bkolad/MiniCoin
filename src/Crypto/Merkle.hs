
module Crypto.Merkle
    (MerkleRoot, mkRoot) where


import           Crypto.Extended
import qualified Crypto.Hash.MerkleTree as MT
import qualified Data.ByteString        as B

type  MerkleRoot = HexBS

-- |
mkRoot :: [B.ByteString] -> MerkleRoot
mkRoot bss =
    let tree = MT.mkMerkleTree bss
    in HexBS $ MT.mtHash tree
