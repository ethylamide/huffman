import System.Environment
import System.IO

import Control.Monad

import Data.Int
import Data.Word
import Data.Maybe

import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQueue
import qualified Data.Map as Map

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Binary
import Data.Binary.Put as Put
import Data.Binary.Get as Get

import qualified Data.Binary.Bits.Put as BPut
import qualified Data.Binary.Bits.Get as BGet

data Freq = Freq Word8 Int
          deriving Show

data Trie = Empty | Node Freq Trie Trie
          deriving Show

type FreqTable = Map.Map Word8 Int
type CodeTable = Map.Map Word8 BS.ByteString

instance Eq Freq where
  (==) (Freq _ a) (Freq _ b) = a == b

instance Ord Freq where
  compare (Freq _ a) (Freq _ b) = a `compare` b

instance Eq Trie where
  (==) Empty Empty = True
  (==) (Node _ _ _) Empty = False
  (==) Empty (Node _ _ _) = False
  (==) (Node a _ _) (Node b _ _) = a == b

instance Ord Trie where
  compare (Node a _ _) (Node b _ _) = a `compare` b
  compare (Node _ _ _) Empty = GT
  compare Empty (Node _ _ _) = LT

freqWord :: Freq -> Word8
freqWord (Freq w _) = w

freqFreq :: Freq -> Int
freqFreq (Freq _ f) = f

trieNull :: Trie -> Bool
trieNull Empty = True
trieNull _     = False

isLeaf :: Trie -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

trieFreq :: Trie -> Maybe Freq
trieFreq (Node f _ _) = Just f
trieFreq Empty        = Nothing

trieLeft :: Trie -> Maybe Trie
trieLeft (Node _ l _) = Just l
trieLeft Empty = Nothing

trieRight :: Trie -> Maybe Trie
trieRight (Node _ _ r) = Just r
trieRight Empty = Nothing

emptyFreqTable :: FreqTable
emptyFreqTable = Map.fromList $ zip [0..255] $ repeat 0

countFrequency :: LBS.ByteString -> FreqTable
countFrequency bs = LBS.foldl (\t w -> Map.adjust (+1) w t) emptyFreqTable bs

insertFreq :: Freq -> MinQueue Trie -> MinQueue Trie
insertFreq x@(Freq _ f) pq = if f > 0
                             then PQueue.insert (Node x Empty Empty) pq
                             else pq

buildPq :: FreqTable -> MinQueue Trie
buildPq t = Map.foldWithKey (\f w pq -> insertFreq (Freq f w) pq) PQueue.empty t

mergeNodes :: MinQueue Trie -> MinQueue Trie
mergeNodes pq = let left@(Node  (Freq _ lf) _ _) = fromJust $ PQueue.getMin pq
                    right@(Node (Freq _ rf) _ _) = fromJust $ PQueue.getMin (PQueue.deleteMin pq)
                    pq' = (PQueue.deleteMin . PQueue.deleteMin) pq
                    parent = Node (Freq 0 (lf + rf)) left right
                in PQueue.insert parent pq'

mergeTrie :: MinQueue Trie -> Trie
mergeTrie pq = if (PQueue.size pq) > 1
               then mergeTrie $ mergeNodes pq
               else fromJust $ PQueue.getMin pq

buildTrie :: FreqTable -> Trie
buildTrie t = let pq = buildPq t
              in mergeTrie pq

buildCodeTable :: Trie -> CodeTable
buildCodeTable tr = buildCodeTableLoop tr Map.empty BS.empty
  where
    buildCodeTableLoop :: Trie -> CodeTable -> BS.ByteString -> CodeTable
    buildCodeTableLoop x st bs
      | trieNull x = st
      | isLeaf x = let ch = freqWord $ fromJust $ trieFreq x
                   in Map.insert ch bs st
      | otherwise = let left  = fromJust $ (trieLeft x)
                        right = fromJust $ (trieRight x)
                        st'   = buildCodeTableLoop left st (BS.snoc bs 0)
                    in buildCodeTableLoop right st' (BS.snoc bs 1)

putTrie :: Trie -> BPut.BitPut ()
putTrie Empty = return ()
putTrie (Node (Freq w _) Empty Empty) = do
  BPut.putBool True
  BPut.putWord8 8 w

putTrie (Node (Freq _ _) l r) = do
  BPut.putBool False
  putTrie l
  putTrie r

compress :: LBS.ByteString -> LBS.ByteString
compress bs = runPut $ BPut.runBitPut $ do
  putTrie root
  BPut.putWord32be 32 $ fromIntegral $ LBS.length bs
  mapM_ (\w -> encodeHuffman (fromJust $ Map.lookup w codeTable)) (LBS.unpack bs)
  where
    freqTable = countFrequency bs
    root = buildTrie freqTable
    codeTable = buildCodeTable root
    encodeHuffman code = do
      mapM_ putCode (BS.unpack code)
    putCode :: Word8 -> BPut.BitPut ()
    putCode 0 = BPut.putBool False
    putCode 1 = BPut.putBool True

expand :: LBS.ByteString -> LBS.ByteString
expand bs = runGet readInput bs
            where
              readInput = BGet.runBitGet $ do
                root <- readTrie
                len <- readLen
                wordList <- replicateM len (decodeWord root)
                return $ LBS.pack wordList
              decodeWord root = do
                w <- wnLeaf root
                return w

wnLeaf :: Trie -> BGet.BitGet Word8
wnLeaf trie
  | isLeaf trie = return $ fromJust $ do
      freq <- trieFreq trie
      return $ freqWord freq
  | otherwise = do
      bit <- BGet.getBool
      case bit of
       True -> wnLeaf $ fromJust $ trieRight trie
       False -> wnLeaf $ fromJust $ trieLeft trie

readLen :: BGet.BitGet Int
readLen = do
  w <- BGet.getWord32be 32
  return (fromIntegral w)

readTrie :: BGet.BitGet Trie
readTrie = do
  l <- BGet.getBool
  if l then readLeaf else readNode
  where
    readLeaf = do
      w <- BGet.getWord8 8
      return $ Node (Freq w 0) Empty Empty
    readNode = do
      left <- readTrie
      right <- readTrie
      return $ Node (Freq 0 0) left right

parse :: [String] -> LBS.ByteString -> LBS.ByteString
parse ["-"] = compress
parse ["+"] = expand

main = do
  op <- getArgs
  content <- LBS.getContents
  LBS.putStr $ (parse op) $ content
