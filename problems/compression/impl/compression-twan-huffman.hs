
-- 0:40 - 1:21

import Data.Char
import Data.List
import Data.Ord
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Control.Applicative
import Data.Bits

--------------------------------------------------------------------------------
-- Base 64 encoding
--------------------------------------------------------------------------------

from64 :: Char -> Int
from64 x
  | x >= 'a' && x <= 'z' = ord x - ord 'a'
  | x >= 'A' && x <= 'Z' = ord x - ord 'A' + 26
  | x >= '0' && x <= '9' = ord x - ord '0' + 26 + 26
  | x == ' '             = 26 + 26 + 10
  | x == '.'             = 26 + 26 + 10 + 1
  | otherwise            = error "invalid base64 char"

to64 :: Int -> Char
to64 x
  | x      < 26           = chr $ x + ord 'a'
  | x - 26 < 26           = chr $ x + ord 'A' - 26
  | x - 26 - 26 < 10      = chr $ x + ord '0' - 26 - 26
  | x - 26 - 26 - 10 == 0 = ' '
  | x - 26 - 26 - 10 == 1 = '.'
  | otherwise            = error "can't encode"


type Bit=Bool
toBits :: Int -> Int -> [Bit]
toBits n x = [ x `testBit` i | i <- [0..n-1] ]

fromBits :: Int -> [Bit] -> Maybe (Int,[Bit])
fromBits n [] = Nothing
fromBits n xs = go 0 0 xs
  where go k i xs | k == n = Just (i,xs)
        go k i (False:xs) = go (k+1) i xs
        go k i (True:xs)  = go (k+1) (i `setBit` k) xs
        go k i [] = Just (i,[]) -- NOT a failure, so we don't lose the last few bits


encode :: [Bit] -> String
encode = map to64 . unfoldr (fromBits 6)

-- note: may have some extra 0 bits at the end!
decode :: String -> [Bit]
decode = concatMap (toBits 6 . from64)


--------------------------------------------------------------------------------
-- Priority Queue
--------------------------------------------------------------------------------

type PQ a = IntMap.IntMap [a]

pqPush :: Int -> a -> PQ a -> PQ a
pqPush n a = IntMap.insertWith (++) n [a]

pqPopMin :: PQ a -> Maybe ((Int,a),PQ a)
pqPopMin pq = do ((n,x:xs),pq') <- IntMap.minViewWithKey pq
                 return ((n,x), if null xs then pq' else IntMap.updateMin tail pq)

--------------------------------------------------------------------------------
-- Huffman
--------------------------------------------------------------------------------

data Tree a
  = Bin (Tree a) (Tree a)
  | Leaf a
 deriving Show

showTree x = Tree.drawTree $ toTree x
toTree (Leaf a) = Tree.Node (show a) []
toTree (Bin a b) = Tree.Node "" [toTree a, toTree b]

-- build a huffman tree
huffman :: [(a,Int)] -> Tree a
huffman xs = go $ IntMap.fromListWith (++) [ (n,[Leaf a]) | (a,n) <- xs ]
  where go :: PQ (Tree a) -> Tree a
        go pq = let Just ((n,a),pq') = pqPopMin pq
                in case pqPopMin pq' of
                  Just ((m,b),pq'') -> go $ pqPush (n+m) (Bin a b) pq''
                  Nothing -> a


huffmanDecode :: Tree a -> [Bool] -> (a, [Bool])
huffmanDecode (Leaf a) xs = (a,xs)
huffmanDecode (Bin  a _) (False:xs) = huffmanDecode a xs
huffmanDecode (Bin  _ b) (True :xs) = huffmanDecode b xs

huffmanMap :: Tree a -> [(a,[Bool])]
huffmanMap (Leaf a)  = [(a,[])]
huffmanMap (Bin a b) = [ (x,False:xs) | (x,xs) <- huffmanMap a ]
                    ++ [ (x,True :xs) | (x,xs) <- huffmanMap b ]

--------------------------------------------------------------------------------
-- Simple huffman compression
--------------------------------------------------------------------------------

-- the huffman table

baseHisto :: Map.Map Char Int
baseHisto = Map.fromList $ [ (x,1) | x <- ['a'..'z']++[' '] ] ++ [('@',0)]
betterHisto = [(' ',100102),('@',0),('a',37620),('b',8306),('c',12887),('d',20227),('e',61901),('f',12273),('g',9492),('h',31974),('i',33426),('j',663),('k',2990),('l',19745),('m',11546),('n',32455),('o',36582),('p',9590),('q',511),('r',28652),('s',30453),('t',43393),('u',13420),('v',4736),('w',10701),('x',695),('y',8971),('z',708)] -- from 77.txt

--theTree = huffman . Map.toList $ baseHisto
theTree = huffman betterHisto

theTable = Map.fromList $ huffmanMap theTree


compress, decompress :: String -> String

-- Compression using table
compress = encode . concatMap (theTable Map.!) . (++"@")

-- De compression
decompress = takeWhile (/='@') . unfoldr (Just . huffmanDecode theTree) . decode

--------------------------------------------------------------------------------
-- Text statistics
--------------------------------------------------------------------------------

oneGrams :: String -> Map.Map Char Int
oneGrams xs = Map.fromListWith (+) $ zip xs (repeat 1)

histo text = Map.unionWith (+) baseHisto $ oneGrams (filter (/='\n') text)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
    mode <- getLine
    input <- getLine
    putStrLn $ case mode of
       "compress" -> compress input
       "decompress" -> decompress input
       _            -> error "unknown mode"


test = "alice was beginning to get very tired of sitting by her sister on the bank and of having nothing to do once or twice she had peeped into the book her sister was reading but it had no pictures or conversations in it and what is the use of a book thought alice without pictures or conversation"
