
import InputGeneration
import Control.Monad
import MathCombinatoricsMultiSet as MS

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

newtype Run = Run [Int]

instance Show Run where
    showsPrec _ (Run xs) = shows (length xs)
                         . showString "\n"
                         . showListWithSep " " shows xs

isOk :: [Int] -> Bool
isOk xs = sum (map fromIntegral xs :: [Integer]) `mod` fromIntegral (length xs) == 0

mkRun xs 
  | isOk xs = Run xs
  | otherwise = error "not evenly divisible by length"

mkSafeRun = mkRun . makeDivisable 
makeDivisable xs
   | n-m >= 0  = fromInteger (n-m) : xs
   | otherwise = fromInteger (2*n-m) : xs
  where total = sum (map fromIntegral xs :: [Integer])
        n = fromIntegral (length xs + 1)
        (_d,m) = total `divMod` n

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

exhaustiveRun n maxV = map Run $ filter isOk $ replicateM n [0..maxV]

exhaustiveRuns = concat $
    [ exhaustiveRun n 5
    | n <- [1..5]
    ]
 ++ [ exhaustiveRun n 3
    | n <- [6..7]
    ]
 ++ [ exhaustiveRun n 1
    | n <- [8..10]
    ]

longSmallRuns = map mkRun $
    (MS.permutations . MS.fromList $ (replicate 50 4) ++ [2,6])
 ++ (MS.permutations . MS.fromList $ (replicate 20 4) ++ [2,2,8])
 ++ (MS.permutations . MS.fromList $ (replicate 20 4) ++ [0,6,6])

incDecRuns = 
    [ mkSafeRun [1..n]
    | n <- [1..10]
    ]
 ++ [ mkSafeRun $ reverse [1..n]
    | n <- [1..10]
    ]
 ++ [ mkRun $ [1..n]
    | n <- [1,3..10]
    ]
 ++ [ mkRun $ reverse [1..n]
    | n <- [1,3..10]
    ]

gapRuns = 
    [ mkRun $ [b+a] ++ replicate 100000 b ++ [b-a]
    | let b = 1
    , a <- [-1,1]
    ]
 ++ [ mkRun $ replicate 100000 3 ++ replicate 100000 5
    , mkRun $ replicate 100000 5 ++ replicate 100000 3
    ]

mkRandomRun n maxV = do
    mkSafeRun <$> take (n-1) <$> getRandomRs (0,maxV)

randomRuns =
     [ runRandom seed $ mkRandomRun 100    1        | seed <- take 10 [0xAFAFAFFE..] ]
  ++ [ runRandom seed $ mkRandomRun 100    100      | seed <- take 10 [0x34591435..] ]
  ++ [ runRandom seed $ mkRandomRun 100    (10^6-1) | seed <- take 10 [0x12354313..] ]
  ++ [ runRandom seed $ mkRandomRun 1000   1000     | seed <- take 5  [0xDEADF00D..] ]
  ++ [ runRandom seed $ mkRandomRun 10000  (10^3-1) | seed <- take 2  [0x99999888..] ]
  ++ [ runRandom seed $ mkRandomRun 100000 (10^2-1) | seed <- take 2  [0x77766555..] ]
 -- ++ [ runRandom seed $ mkRandomRun 999999 (10^9-1) | seed <- take 1  [0x45143514..] ]

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = printRuns runs
  where runs = incDecRuns ++ exhaustiveRuns ++ longSmallRuns ++ gapRuns ++ randomRuns
