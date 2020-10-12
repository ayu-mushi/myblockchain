module Block
     where
import qualified Data.ByteString as BS(ByteString, singleton, pack, unpack, empty)
import qualified Data.ByteString.Char8 as Char8(pack, unpack)
import Numeric (readHex, showHex)
import qualified Crypto.Hash.SHA256 as SHA256(hash)
import Data.Monoid((<>))
import Data.Word8(Word8)
import Data.Bits (shiftR, (.&.))
import Data.List as List(unfoldr)
import Data.Time (getCurrentTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), hoursToTimeZone, localTimeToUTC)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime)


type Nonce = Int
type Difficulty = Int

data Block = Block {
  b_index :: Int
  , b_previousHash :: BS.ByteString
  , b_timestamp :: UTCTime
  , b_data :: String
  , b_hash :: BS.ByteString
  , b_nonce :: Nonce
  , b_difficulty :: Difficulty
  } deriving (Show, Eq)

type BlockChain = [Block]


initialBlock :: UTCTime -> Block
initialBlock now = Block {
  b_index = 0
  , b_previousHash = BS.singleton 0
  , b_timestamp = now
  , b_data="my genesis block!!"
  --,b_hash = blockHash $ initialBlock now
  , b_hash = genHash (b_index i) (b_previousHash i) (b_timestamp i) (b_data i) (b_nonce i)
  , b_nonce = proofOfWork (b_difficulty i) (b_index i) (b_previousHash i) (b_timestamp i) (b_data i)
  , b_difficulty = defaultDifficulty
  }
  where
    i = initialBlock now

showBlock :: Block -> String
showBlock b = "{"
  ++ "  index:" ++ (show $ b_index b)
  ++ "\n,   timestamp: " ++ show (b_timestamp b)
  ++ "\n,   data: " ++ b_data b
  ++ "\n,   hash: " ++ (bsToHex $ b_hash b)
  -- ++ "\n,   hash_length:" ++ (show $ length $ bsToHex $ b_hash b)
  ++ "\n,   nonce:" ++ show (b_nonce b)
  ++ "\n}"

hexToBs :: String -> BS.ByteString
hexToBs str = BS.pack $ hexToBs' str
  where
    hexToBs' :: String -> [Word8]
    hexToBs' [] = []
    hexToBs' [a] = (fst $ head $ readHex [a, '\0']):hexToBs' str
    hexToBs' (a:b:str) = (fst $ head $ readHex [a, b]):hexToBs' str

nextBlock :: UTCTime -> String -> Block -> Block
nextBlock now blockData block =
  let new_index = (b_index block) + 1
      new_previousHash = blockHash block
      new_timestamp = now
      new_data = blockData
   in Block {
  b_index = new_index
  ,b_previousHash = new_previousHash
  ,b_timestamp = new_timestamp
  ,b_data= new_data
  ,b_hash = blockHash $ nextBlock now blockData block
  ,b_nonce = proofOfWork (b_difficulty block) new_index new_previousHash new_timestamp new_data
  ,b_difficulty = b_difficulty block
  }

blockHash :: Block -> BS.ByteString
blockHash Block {b_index = i, b_previousHash = p, b_timestamp = t, b_data = d, b_nonce = n}= genHash i p t d n

genHash :: Int -> BS.ByteString -> UTCTime -> String -> Nonce -> BS.ByteString
genHash index previousHash timestamp _data nonce = SHA256.hash $ Char8.pack $ show (index, previousHash, timestamp, _data, nonce)

proofOfWork :: Difficulty -> Int -> BS.ByteString -> UTCTime -> String -> Nonce
proofOfWork difficulty index previousHash timestamp _data = proofOfWork' 0
  where
    proofOfWork' nonce =
      if isGoodHash difficulty $ genHash index previousHash timestamp _data nonce
         then nonce
         else proofOfWork' (nonce + 1)

isGoodHash :: Difficulty -> BS.ByteString -> Bool
isGoodHash difficulty hash = all (== '0') $ take difficulty $ bsToHex $ hash


bsToHex :: BS.ByteString -> String
bsToHex x = concat $ map (`showHex` "") $ concat $ map split8to4 $ BS.unpack x
  where
    split8to4 x = [(x .&. 0xF0) `shiftR` 4, x .&. 0x0F]


verifyHash :: Block -> BS.ByteString -> Bool
verifyHash verified verifier = (blockHash verified) == verifier

verifyBlock :: Block -> Block -> Bool
verifyBlock previous new =
  ((b_index previous) + 1) == (b_index new)
  && (b_hash previous) == (b_previousHash new)
  && (blockHash new) == (b_hash new)
  && isGoodHash (b_difficulty new) (b_hash new)
  && b_difficulty new == b_difficulty previous

verifyChain :: Block -> [Block] -> Bool
verifyChain initial blockchain = verifyChain' blockchain
  where
    verifyChain' [] = True
    verifyChain' [a] = (a == initial)
    verifyChain' (a:b:chain) = (verifyBlock b a) && (verifyChain' (b:chain))

addBlock :: Difficulty -> Block -> [Block] -> Maybe [Block]
addBlock difficulty new (previous:bs) =
  if verifyBlock previous new
     then Just $ new:previous:bs
     else Nothing

defaultDifficulty :: Difficulty
defaultDifficulty = 2

reChain :: Block -> [Block]
reChain initialBlock = List.unfoldr (\b -> Just (b, nextBlock time1 "" b)) initialBlock

time1 :: UTCTime
time1 = now
  where
    tz = hoursToTimeZone 9
    lt = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 6 0 0)
    now = localTimeToUTC tz lt


makeChain :: Block -> Int -> [Block]
makeChain initial n = reverse $ take n $ reChain initial

scanChain :: Block -> [(UTCTime, String)] -> [Block]
scanChain initial = scanr (\(time, _data) -> \block -> nextBlock time _data block) initial
