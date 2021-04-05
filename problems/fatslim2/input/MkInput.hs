
import InputGeneration
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Arrow
import Data.Ord

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

dropTail n = reverse . drop n . reverse

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

type Time = Int
type Duration = Int
type Person = (Time,Duration)
data Run = Run Int Int [Person]

instance Show Run where
    showsPrec _ (Run a b xs)
      | otherwise = shows a . showString " " . shows b . showString "\n"
                  . shows (length xs)
                  . showString "\n"
                  . showListWith (\(t,d) -> shows t . showString " " . shows d . showString "\n") xs

mkRun a b xs = Run a b (sortBy (comparing fst) xs)

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

solve :: Int -> Int -> [Person] -> Int
solve n_g n_t = count (<= 4 * 60 * 60) . accessGates n_g . ticketMachines n_t

solveRun :: Run -> Int
solveRun (Run n_g n_t xs) = length xs - solve n_g n_t xs

endTimes :: Run -> [Int]
endTimes (Run n_g n_t xs) = sort . accessGates n_g . ticketMachines n_t $ xs

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

buyTicket :: Person -> (Int, Maybe (Int -> Int))
buyTicket (t,0)  = (t, Nothing)
buyTicket (t,dt) = (t, Just (+dt))

passGate :: Int -> (Int, Maybe (Int -> Int))
passGate t = (t, Just (+1))

ticketMachines :: Int -> [Person] -> [Int]
ticketMachines n = sort . simulateQueues buyTicket (replicate n 0)

accessGates :: Int -> [Int] -> [Int]
accessGates n = simulateQueues passGate (replicate n 0)

simulateQueues :: Ord a => (b -> (a, Maybe (a -> a))) -> [a] -> [b] -> [a]
simulateQueues _  _          []         = []
simulateQueues dt qqs@(q:qs) (x:xs) = case dt x of
     (a, Nothing) -> a : simulateQueues dt qqs xs
     (a, Just da) -> let a' = da (max a q)
                     in  a' : simulateQueues dt (insert a' qs) xs

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

endtime = 4*60*60

sameTimeRuns =
    [ mkRun a b (replicate n td)
    | n <- [1..30]
    , (a,b) <- [(1,1),(1,2),(2,1),(3,3),(9,9)]
    , td <- [(123,456),(123,1),(123,0),(endtime-5,0),(endtime-5,1)]
    ]

makeItFit :: Int -> Run -> Run
makeItFit except run@(Run a b xs) = Run a b [(t+endtime-endT,dt) | (t,dt) <- xs]
  where endT = min endtime . maximum . (0:) . dropTail except . endTimes $ run

mkRandomRun n maxT maxDT = do
    a   <- getRandomR (1,9)
    b   <- getRandomR (1,9)
    ts  <- getRandomRs (1,maxT)
    dts <- getRandomRs (-maxDT,maxDT)
    pf  <- getRandomR (False,True)
    f   <- getRandomR (1,n)
    return $ (if pf then makeItFit f else id) $ mkRun a b (take n $ zip ts (map (max 0) dts))


randomRuns =
     [ runRandom seed $ mkRandomRun 100    1000  100   | seed <- take 10 [0xAFAFAFFE..] ]
  ++ [ runRandom seed $ mkRandomRun 123    10    10    | seed <- take 10 [0x12354313..] ]
  ++ [ runRandom seed $ mkRandomRun 10     2     2     | seed <- take 100 [0x45143514..] ]
  ++ [ runRandom seed $ mkRandomRun 10     10000 1800  | seed <- take 100 [0x98765432..] ]
  ++ [ runRandom seed $ mkRandomRun 1000   1000  2     | seed <- take 5  [0xDEADF00D..] ]
  ++ [ runRandom seed $ mkRandomRun 10000  1000  1     | seed <- take 2  [0x99999888..] ]
  ++ [ runRandom seed $ mkRandomRun 100000 1000  100   | seed <- take 1  [0x77766555..] ]
 -- ++ [ runRandom seed $ mkRandomRun 999999 (10^9-1) | seed <- take 1  [0x45143514..] ]


--------------------------------------------------------------------------------
-- Order matters!
--------------------------------------------------------------------------------

orderMatters =
     [ mkRun 1 1 ps
     | ps <- permutations' [(endtime-11,0),(endtime-11,10),(endtime-11,11)]
     ]
  ++ [ mkRun 1 1 ps
     | ps <- permutations' [(endtime-1,0),(endtime-11,9),(endtime-11,11)]
     ]

--------------------------------------------------------------------------------
-- Random dense runs
--------------------------------------------------------------------------------

{-
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
-}
--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = printRunsNoBr runs
runs = sameTimeRuns ++ orderMatters ++ randomRuns
