
-- 17:10 - 18:06  (inclusief chatten, foutjes in voorbeeld, enz.)

import Control.Monad
import Control.Applicative
import Data.List
import Data.Ord
import Numeric

epsilon = 1e-10
warnEps = 1e-6

main :: IO ()
main = flip replicateM_ run =<< readLn

run :: IO ()
run = do mines <- readInput
         case solve mines of
           (a,_) -> putStrLn $ ($[]) . showFFloat (Just 5) $ a

readInput :: IO [Point]
readInput = do
    n <- readLn
    replicateM n $ do [x,y] <- map read . words <$> getLine
                      return (fromInteger x, fromInteger y)

type Point = (Double,Double)
data Point' = P { r :: Double, theta :: Double }
instance Show Point' where show (P r t) = show (r,t)

toPoint' :: Point -> Point'
toPoint' (x,y) = P r theta
  where r     = sqrt $ x^2 + y^2
        theta = toDegree $ atan2 y x

twice x = [x,x+360]

wedgePoints  = sort . concatMap twice . map (theta . toPoint')

toDegree = (*(180/pi))

solveW :: [Double] -> [(Double,Double)]
solveW (x:xs@(y:_)) = (y-x,(x+y)/2) : solveW xs
solveW _ = []

solve = maximum . solveW . wedgePoints

example1 = [(2.0,2.0),(2.0,-2.0),(-2.0,-2.0)]
example2 = [(4.0,-1.0),(4.0,2.0),(1.0,4.0),(-2.0,4.0),(-4.0,1.0),(-4.0,-2.0),(-1.0,-4.0),(2.0,-4.0)]
example3 = [(4.0,-1.0),(4.0,2.0),(1.0,4.0),(-2.0,4.0),(-4.0,2.0),(-4.0,-2.0),(-1.0,-4.0),(2.0,-4.0)]
example4 = [(-9.0,-1.0),(-4.0,3.0),(-4.0,-3.0),(-1.0,-6.0),(2.0,4.0),(3.0,-4.0),(4.0,-2.0),(7.0,-1.0)]
