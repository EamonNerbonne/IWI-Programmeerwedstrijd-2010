
import InputGeneration
import Data.Char
import System.Environment

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

data Run = Run [Int] [ShowS]

instance Show Run where
    showsPrec _ (Run xs zs) = shows (length xs) . showString " " . shows (length zs)
                         . showString "\n"
                         . showListWithSep "\n" id zs

zs xs =      [ if d > 0
                              then shows i . showString " " . shows 0 . showString " " . shows d
                              else shows 0 . showString " " . shows i . showString " " . shows (-d)
                            | (i,d) <- zip [1..] (tail xs), d /= 0 ]

--------------------------------------------------------------------------------
-- Test cases : exhaustive
--------------------------------------------------------------------------------

exhaustiveRuns =
  [ Run xs z
  | n <- [1..6]
  , xs <- replicateM n [-3..7]
  , sum xs == 0
  , let z = (zs xs)
  , not (null z)
  ]

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = printRuns runs

runs = exhaustiveRuns
