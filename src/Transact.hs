{-# LANGUAGE PackageImports #-}
module Transact where
import qualified Crypto.Hash.SHA256 as SHA256(hash)
import qualified Data.ByteString as BS(ByteString, singleton, pack, unpack, empty)
import qualified Data.ByteString.Char8 as Char8(pack)
import ECDSA
import "crypto-api" Crypto.Random(CryptoRandomGen, GenError)

data TxOut = TxOut {
  address :: String
  ,amount :: Int
  }deriving (Show)

data TxIn = TxIn {
  txOutId :: String
  ,txOutIndex :: Int
  ,signature :: String
  }deriving (Show)

data Transact = Transact {
  txId :: BS.ByteString
  , txIns :: [TxIn]
  , txOuts :: [TxOut]
  } deriving (Show)

data UnspentTxOut = UnspentTxOut {
  utxOutId :: String,
  utxOutIndex :: Int,
  utxAddress :: String,
  utxAmount :: Int
  }

getTransanctId :: Transact -> BS.ByteString
getTransanctId
  (Transact {
    txIns = ins,
    txOuts = outs})
  = SHA256.hash $ Char8.pack $
    show (map mapf ins
         , outs)
  where
    mapf x = case x of
                  TxIn{txOutId = _txOutId, txOutIndex = _txOutIndex}
                    -> (_txOutId,_txOutIndex)


signTxIn  :: CryptoRandomGen g => Transact -> Int -> PrivateKey -> TxOut -> g->Either GenError (Signature, g)
signTxIn transanct txInIndex privateKey aUnspentTxOuts g
  = let txIn = (txIns transanct) !! txInIndex
        dataToSign = txId transanct
        key = undefined
        signature = sign privateKey dataToSign g
     in signature
