
import InputGeneration
import System.Environment

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

newtype Run = Run [(Int,Int)]

instance Show Run where
    showsPrec _ (Run xs) = shows (length xs)
                         . showString "\n"
                         . showListWith (\(a,b) -> shows a . showString " " . shows b . showString "\n") xs

mkRun = Run . fastNub

runLength (Run xs) = length xs

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

diagRuns = concat
    [ [mkRun pts, mkRun (reverse pts)]
    | n <- [1..30]++[200000]
    , let pts = [(x,x) | x <- [1..n] ]
    ]

codiagRuns = concat
    [ [mkRun pts, mkRun (reverse pts)]
    | n <- [1..30]++[200000]
    , let pts = [(x,n-x+1) | x <- [1..n] ]
    ]

exhaustiveRuns = 
    [ mkRun whichPts
    | n <- [1..3]
    , let pts = [(x,y) | x <- [1..n], y <- [1..n] ]
    , whichPts <- subsequences' pts
    ]

mkRandomRun n maxXY = do
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

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = do
     args <- getArgs
     case args of
       []        -> printRunsNoBr runs
       ["licht"] -> printRunsNoBr (filter ((< 10^4) . runLength) runs)
       _ -> error "Usage: MkInput [licht]"
  where runs = diagRuns ++ codiagRuns ++ exhaustiveRuns ++ randomRuns
