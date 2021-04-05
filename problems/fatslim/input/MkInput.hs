
import InputGeneration
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Arrow

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

type Time = Int
type Duration = Int
data Slot = L | XL deriving (Eq,Ord,Show,Enum)
type Person = (Time,Duration,Slot)
data Run = Run Int Int [Person]

instance Show Run where
    showsPrec _ (Run a b xs)
      | a > b     = error "wrong costs"
      | otherwise = shows a . showString " " . shows b . showString "\n"
                  . shows (length xs)
                  . showString "\n"
                  . showListWith (\(t,d,s) -> shows t . showString " " . shows d . showString " " . shows s . showString "\n") xs

mkRun = Run

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

sameTimeRuns =
    [ mkRun a b (replicate n (123,456,fat))
    | n <- [1..30]
    , (a,b) <- [(5,5),(5,10),(5,15)]
    , fat <- [L,XL]
    ]

mkSeries :: Time -> [(Slot,Duration)] -> [Person]
mkSeries _ [] = []
mkSeries t ((slot,d):slots) = (t,d,slot) : mkSeries (t+d) slots

seriesRuns =
    [ mkRun 1243 6452 $ mkSeries 1234567 []
    , mkRun 1243 6452 $ mkSeries 4645626 [(L,2)]
    , mkRun 1243 6452 $ mkSeries 6776352 [(L,2),(XL,1)]
    , mkRun 1243 6452 $ mkSeries 2456251 [(XL,2),(L,987643)]
    ]

{-
mkExhaustive maxD maxT 0 = [[]]
mkExhaustive maxD maxT n = []
-}

exhaustivePerson :: Duration -> Time -> [Person]
exhaustivePerson maxD maxT = [(t,d,s) | t <- [0..maxT], d <- [1..maxD], s <- [L,XL] ]

exhaustiveRuns =
    [ mkRun a b ps
    | n <- [1,2,3,4,5]
    , (a,b) <- [(1,1),(1,2),(1,3),(2,3),(1,4),(3,4)]
    , slots <- replicateM n [L,XL]
    , ps <- exhaustivePersons slots
    ]

exhaustivePersons :: [Slot] -> [[Person]]
exhaustivePersons slots = go 0 slots []
  where
    go :: Time -> [Slot] -> [Time -> Person] -> [[Person]]
    go t [] [] = [[]]
    --go t fresh open = goFresh t fresh open ++ goClose t fresh open
    go t fresh open = goFresh t fresh open ++ goCloseMany t fresh open
    goFresh t []      []  = [[]]
    goFresh t []      _   = []
    goFresh t (x:xs) open = go (t+1) xs ((\end -> (t,end-t,x)) : open) -- open a slot x
    goClose t fresh open = [ x t : rest
                           | (x,xs) <- pick open
                           , rest <- go (t+1) fresh xs
                           ]
    goCloseMany t fresh open = [ map ($ t) xs ++ rest
                               | (xs,ys) <- partitions open -- the order of closing doesn't matter
                               , rest <- goFresh (t+1) fresh ys
                               ]

pick :: [a] -> [(a,[a])]
pick [] = []
pick (x:xs) = (x,xs) : map (second (x:)) (pick xs)

partitions :: [a] -> [([a],[a])]
partitions []      =  []
partitions (x:xs)  =  ([x],xs) : foldr f [] (partitions xs)
  where f ys r = (first (x:) ys) : (second (x:) ys) : r


-- permutations where each item appears twice,

{-
--TODO
mkRandomRun n maxT = do
    xs <- getRandomRs (1,maxXY)
    ys <- getRandomRs (1,maxXY)
    let ps = take n $ fastNub $ zip xs ys
    return $ Run ps

randomRuns =
     [ runRandom seed $ mkRandomRun 100    100      | seed <- take 10 [0xAFAFAFFE..] ]
  ++ [ runRandom seed $ mkRandomRun 100    (10^9-1) | seed <- take 10 [0x12354313..] ]
  ++ [ runRandom seed $ mkRandomRun 1000   1000     | seed <- take 5  [0xDEADF00D..] ]
  ++ [ runRandom seed $ mkRandomRun 10000  (10^9-1) | seed <- take 2  [0x99999888..] ]
  ++ [ runRandom seed $ mkRandomRun 100000 (10^9-1) | seed <- take 1  [0x77766555..] ]
 -- ++ [ runRandom seed $ mkRandomRun 999999 (10^9-1) | seed <- take 1  [0x45143514..] ]
-}

--------------------------------------------------------------------------------
-- Random dense runs
--------------------------------------------------------------------------------

type PQ a = IntMap.IntMap [a]

mkDense :: Time -> PQ Slot -> [Slot] -> [Duration] -> [Person]
mkDense _ _ _ [] = []
mkDense t filled [] durations = mkDense (max t t') more slots durations
   where Just ((t',slots),more) = IntMap.maxViewWithKey filled
mkDense t filled (slot:slots) (d:durations) = (t,d,slot) : mkDense t filled' slots durations
   where filled' = IntMap.insertWith (++) (t+d) [slot] filled

mkDense_ = mkDense 0 IntMap.empty

denseRuns =
    [ mkRun 5 5  $ mkDense_ [L]     [100,123,54,143,61043,34,61,601,401,4,104,105,104,13]
    , mkRun 5 6  $ mkDense_ [L,XL]  [100,123,54,143,61043,34,61,601,401,4,104,105,104,13]
    , mkRun 5 6  $ mkDense_ [L,XL]  (replicate 1000 10)
    , mkRun 5 15 $ reverse $ mkDense_ [XL,L]  (replicate 1000 10)
    , mkRun 5 15 $ mkDense_ [XL,L,L,XL,XL,L,XL] (replicate 1000 10 ++ replicate 20 30 ++ [1..50])
    , mkRun 5 7  $ mkDense_ (replicate 40 L++[XL]) [1..1000]
    ]

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = printRunsNoBr runs
runs = sameTimeRuns ++ seriesRuns ++ exhaustiveRuns ++ denseRuns
