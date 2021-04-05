-- 14:38
-- 19:07
-- 10:30

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Either

--------------------------------------------------------------------------------
-- input
--------------------------------------------------------------------------------

type Time = Int
data Slot = L|XL deriving (Eq,Ord,Show,Read,Enum)
data Person = Person Time Time Slot deriving (Eq,Ord,Show)

readPerson :: String -> Person
readPerson xs = case words xs of
                  [a,b,c] -> Person (read a) (read b) (read c)

--------------------------------------------------------------------------------
-- Priority Queue
--------------------------------------------------------------------------------

type PQ a = IntMap.IntMap [a]

{-
pqPush :: Int -> a -> PQ a -> PQ a
pqPush n a = IntMap.insertWith (++) n [a]
-}
pqPush :: Int -> a -> PQ a -> PQ a
pqPush n x = IntMap.alter alt n 
  where alt Nothing = Just [x]
        alt (Just xs) = Just (x:xs)

pqPopMin :: PQ a -> Maybe ((Int,a),PQ a)
pqPopMin pq = do ((n,x:xs),pq') <- IntMap.minViewWithKey pq
                 return ((n,x), if null xs then pq' else IntMap.updateMin tail pq)

pqFromList :: [(Int,[a])] -> PQ a
pqFromList = IntMap.fromListWith (++)

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

type Event = (Time,Slot,Bool)
type Nr   = Int
type Cost = Int

events :: [Person] -> [Event]
events ps = sort [ x | Person t d s <- ps, x <- [(t,s,True),(t+d,s,False)] ]


bestCost :: [Cost] -> [[Nr]] -> Cost
bestCost xs []  = error "no solution"
bestCost xs yss = minimum (map (dot xs) yss)

dot xs ys = sum $ zipWith (*) xs ys

solveBest :: [Cost] -> [Person] -> Cost
solveBest costs = bestCost costs . solve


{-
-- solution method 1: try + binary search

possible :: Map Slot Int -> [Person] -> Bool
possible slots ps = go slots $ events ps
   where go avail [] = True
         go avail ((_,s,True):xs) = case Map.findWithDefault 0 s avail of
                                       0 -> False -- past niet
                                       n -> go (Map.insert s (n-1) avail) xs
         go avail ((_,s,False):xs) = go (Map.adjust succ s avail) xs

infMultipleList :: [Int] -> [[Int]]
infMultipleList xs = go [(0,map 0 xs)]
 where go (y:ys) = y : 

infMultipleList :: (a -> a) -> [[Int]]
infMultipleList xs = go [(0,map 0 xs)]
 where go (y:ys) = y : 
-}

{-
-- solution method 2: parallel solution

solve' :: [Person] -> [[Nr]] -> [[Nr]]
solve' []               sols = sols
solve' ((_,s,False):xs) sols = solve' xs $ map (incSlot s) sols
solve' ((_,s,True):xs)  sols = solve' xs $ nubSorted $ concatMap (decSlots s) sols

decSlot :: Slot -> [Nr] -> Maybe [Nr]
decSlot slot xs
  | v == 0    = Nothing (us,v:vs)
  | otherwise = Just (us,v:vs)
 where (us,v:vs) = splitAt (toEnum slot) xs

decSlots :: Slot -> [Nr] -> Maybe [Nr]
decSlots slot xs
  | v == 0    = Nothing (us,v:vs)
  | otherwise = Just (us,v:vs)
 where (us,v:vs) = splitAt (toEnum slot) xs
-}

{-
incSlot :: Slot -> [Nr] -> [Nr]
incSlot slot xs = (us,v+1:vs)
 where (us,v:vs) = splitAt (toEnum slot) xs
-}

--------------------------------------------------------------------------------
-- Solution (2)
--------------------------------------------------------------------------------

type State = ([Nr], PQ (Slot,Maybe Int))

initEvents :: [Person] -> PQ (Slot,Maybe Int)
initEvents ps = pqFromList [ (t,[(s,Just d)]) | Person t d s <- ps ]


solve :: [Person] -> [[Nr]]
solve ps = solveStates [([0,0],initEvents ps)]

solveStates :: [State] -> [[Nr]]
solveStates [] = []
solveStates ss = ns' ++ solveStates ss''
  where (ss',ns') = partitionEithers $ map step ss
        ss'' = Map.toList . Map.fromList $ concat ss'

step :: State -> Either [State] [Nr]
step (free,events) = case pqPopMin events of
   Nothing                    -> Right free
   Just ((t,(s,Nothing)),evs) -> case splitAt (fromEnum s) free of
                                   (us,i:vs) -> Left [(us++[i+1]++vs, evs)]
   Just ((t,(s,Just d)), evs) -> Left [(free', pqPush (t+d) (s',Nothing) evs)
                                      | (_,ds,free') <- decSlot (fromEnum s) free
                                      , let s' = toEnum ds :: Slot
                                      ]

decSlot :: Int -> [Nr] -> [(Bool,Int,[Nr])]
decSlot pos [] = []
decSlot pos (i:vs)
  | pos <= 0  = [ (i==0, 0, max (i-1) 0 : vs) | pos == 0 || i/=0 ] -- it fits, or add a new slot
             ++ [ (upg, ds+1, 0:vs') | i == 0, (upg,ds,vs') <- decSlot (pos-1) vs ] -- try a larger slot
  | otherwise = [ if upg && i > 0
                    then (False,ds+1,(i-1):vs') -- instead of adding a new slot, upgrade an existing one
                    else (upg,ds+1,i:vs')
                | (upg,ds,vs') <- decSlot (pos-1) vs ]


{-

valid :: State -> Maybe [Nr]
valid (S free events) = case IntMap.minViewWithKey events of
   Nothing -> Just free
   Just ((t,evs),next) -> handleEvents t (sort evs) next
  where handleEvents t [] st = valid st
        handleEvents t ((s,Nothing):xs) (S free events) = handleEvents t xs st{sFree = incSlot s free}
        handleEvents t ((s,Just d) :xs) (S free events) = case splitAt (fromEnum s) free of
                                                            (us,i:vs) | i > 0 -> handleEvents t xs st{sFree = us++[i-1]++vs}
                                                            (us,0:vs)         -> -- try a larger slot
                                                                                 -- add a new slot
                                                                                 -- 
   --Just (t,(s,Nothing))
   --Just (t,(s,Just d))



initEvents :: [Person] -> IntMap.IntMap ([Slot],[(Slot,Int)])
initEvents ps = IntMap.fromListWith [ (t,([],[(s,d)])) | (t,d,s) <- ps ]
-}

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

run = do
    [ca,cb] <- map read . words <$> getLine
    n <- readLn
    persons <- replicateM n (readPerson <$> getLine)
    let sol = solveBest [ca,cb] persons
    print sol

main = flip replicateM_ run =<< readLn

