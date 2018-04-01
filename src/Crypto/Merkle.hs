
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Crypto.Merkle where

{-
import qualified Crypto.Hash.MerkleTree as MT
import qualified Data.ByteString as B
import           Data.Binary
import           Data.Aeson             (FromJSON (..), ToJSON (..), withObject,withText, object, (.=))
import           Data.Aeson.TH
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
--import Crypto.Hex


type MerkleTree = MT.MerkleTree B.ByteString
type MerkleRoot =  MT.MerkleRoot B.ByteString

-- | Orphant instances
instance Binary (MT.MerkleRoot B.ByteString)
instance Binary (MT.MerkleNode B.ByteString)
instance Binary (MT.MerkleTree B.ByteString)


-- MerkleRoot hash in rypto.Hash.MerkleTree is HexEncoded
instance ToJSON (MT.MerkleRoot B.ByteString) where
    toJSON mRoot = toJSON $ T.decodeLatin1 (decode mRoot)
    --(MT.MerkleRoot mRoot) = toJSON (T.decodeLatin1 mRoot)


instance FromJSON (MT.MerkleRoot B.ByteString) where
    parseJSON = withText "MerkleRoot" $ pure . MT.MerkleRoot . T.encodeUtf8



instance ToJSON (MT.MerkleNode B.ByteString) where
    toJSON (MT.MerkleBranch root left right) =
         object [ "root" .= toJSON root
                , "left" .= toJSON left
                , "left" .= toJSON right
                ]

    toJSON (MT.MerkleLeaf root value) =
        object [ "root" .= toJSON root
               , "left" .= toJSON value
               ]


-- |
mkMerkleTree :: [B.ByteString] -> MerkleTree
mkMerkleTree = mkMerkleTree

-- |
mtRoot :: MerkleTree -> MerkleRoot
mtRoot = mtRoot

mkRootAndTree :: [B.ByteString] -> (MerkleRoot, MerkleTree)
mkRootAndTree bss =
    let tree = mkMerkleTree bss
        root = mtRoot tree
    in (root, tree)--}
