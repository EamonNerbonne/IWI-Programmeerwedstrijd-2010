import Control.Monad
import Control.Applicative
import Data.List
import Data.Array

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

solve :: Int -> [(Int,Int,Int)] -> Bool
solve n = hasZeroSumSubset . elems . accumArray (+) 0 (0,n-1) . concatMap debt
  where debt (a,b,c) = [(a,-c),(b,c)]


hasZeroSumSubset :: [Int] -> Bool
hasZeroSumSubset = go False False 0
  where go a b i xxs = (a && b && i == 0) || case xxs of
                           [] -> False
                           x:xs -> go True b i xs || go a True (i+x) xs

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

readDebts :: String -> (Int,Int,Int)
readDebts xs = case map read $ words xs of [a,b,c] -> (a,b,c)
                                           _ -> error "invoer klopt niet"

run, main :: IO ()
run = do
    [n,m] <- map read . words <$> getLine
    debts <- replicateM m (readDebts `fmap` getLine)
    putStrLn (if solve n debts then "loose" else "tight")

main = flip replicateM_ run =<< readLn
