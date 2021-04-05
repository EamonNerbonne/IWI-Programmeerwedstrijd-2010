-- 16:41 - 17:01 

import Data.List
import Data.Ord

type Poly = [Double]

infinity = 1/0

eval :: Poly -> Double -> Double
eval us x
  | isInfinite x = error "infinite" -- eval us (signum x * 1e100)-- asymptotic behaviour: 
eval us x = sum $ zipWith (*) us (iterate (*x) 1)

derivative :: Poly -> Poly
derivative = zipWith (*) [1..] . drop 1

integral :: Poly -> Poly
integral = (0:) . zipWith (flip (/)) [1..]

integrate :: Poly -> Double -> Double -> Double
integrate p a b = eval (integral p) b - eval (integral p) a

showPoly :: Poly -> String
showPoly [] = "0"
showPoly us = intercalate " + " $ filter (not . null) $ zipWith term [0..] us
  where term _ 0 = ""
        term 0 x = show x
        term 1 x = show x ++ " x"
        term n x = show x ++ " x^" ++ show n

zeroes :: Poly -> [Double]
zeroes []  = [] -- error "zeroes of 0th order polynomial"
zeroes [_] = [] -- error "zeroes of 1st order polynomial"
zeroes [a,b]
  | b == 0    = []
  | otherwise = [-a / b]
zeroes p = sort $ concat $ zipWith (bisectionZero p) between (tail between)
  where between = [-large] ++ zeroes (derivative p) ++ [large]
        large   = 1e10 -- This is tricky!!

bisectionZero :: Poly -> Double -> Double -> [Double]
bisectionZero p a b
   | pa == 0          = [a]
   | pb == 0          = [b]
   | pa < 0 && pb > 0 = [go a b]
   | pb < 0 && pa > 0 = [go b a]
   | otherwise        = []
 where pa = eval p a
       pb = eval p b
       go x y
          | x == y || x == z || y == z = x
          | pz == 0   = z
          | pz < 0    = go z y
          | otherwise = go x z
         where z = (x + y) / 2
               pz = eval p z


-- 17:01 - 17:26

newtype P = P [Double] deriving Eq

instance Show P where
    show (P xs) = showPoly xs

instance Num P where
    fromInteger x = P [fromInteger x]
    P a + P b     = P (addPoly a b)
    negate (P xs) = P (map negate xs)
    abs           = error "abs"
    signum        = error "signum"

class Eval a where
    eval_ :: a -> Double -> Double

addPoly :: Poly -> Poly -> Poly
addPoly [] ys = ys
addPoly xs [] = xs
addPoly (x:xs) (y:ys) = (x+y) : addPoly xs ys

intersections :: Poly -> Poly -> [Double]
intersections a b = zeroes (addPoly a (map negate b))

--

newtype MinPoly = MinPoly {getMinPoly :: [Poly] }

instance Show MinPoly where
   show (MinPoly ps) = "Min[" ++ intercalate "," (map showPoly ps) ++ "]"

evalMP :: MinPoly -> Double -> Double
evalMP (MinPoly ps) x = minimum $ map (flip eval x) ps

integrateMP :: MinPoly -> Double -> Double -> Double
integrateMP polys a b = sum $ zipWith step ps (tail ps)
  where ps = sort $ [a,b] ++ filter (\x -> x > a && x < b) (intersectionsMP polys)
        step u v = integrate lowest u v
           where mid = (u+v) / 2
                 lowest = minimumBy (comparing (flip eval mid)) (getMinPoly polys)

intersectionsMP :: MinPoly -> [Double]
intersectionsMP (MinPoly xs) = nub $ sort $ concat [ intersections x y | (x:ys) <- tails xs, y <- ys ]
