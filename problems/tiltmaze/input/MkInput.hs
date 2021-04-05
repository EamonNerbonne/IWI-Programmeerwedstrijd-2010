
import InputGeneration
import Control.Monad

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

newtype Run = Run [String]

instance Show Run where
    showsPrec _ (Run xs) = shows (length xs) . showString " " . shows (length $ head xs)
                         . showString "\n"
                         . showListWithSep "\n" showString xs

--------------------------------------------------------------------------------
-- Test cases
--------------------------------------------------------------------------------

putSome1 :: Eq a => a -> a -> [a] -> [[a]]
putSome1 a b [] = []
putSome1 a b (x:xs) = [ b:xs | x == a ] ++ map (x:) (putSome1 a b xs)

putSome :: Eq a => a -> a -> [[a]] -> [[[a]]]
putSome a b [] = []
putSome a b (x:xs) = [ y:xs | y <- putSome1 a b x ]
                  ++ map (x:) (putSome a b xs)

exhaustiveRuns = 
    [ Run maze''
    | h <- [1..3]
    , w <- [1..4]
    , maze <- replicateM h (replicateM w ".X")
    , maze'  <- putSome '.' 'A' (maze :: [String])
    , maze'' <- putSome '.' 'B' maze'
    ]

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = printRuns runs
  where runs = exhaustiveRuns
