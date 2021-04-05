{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-binds -fno-warn-type-defaults #-}

import Numeric
import Data.List
import Data.Ord
import Data.Ratio
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Applicative
import System.Random
import Data.Array.ST hiding (range,newArray)
import Data.STRef
import Control.Monad
import Control.Monad.ST

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | 'Concatenate' a list of @ShowS@s
catShows :: [ShowS] -> ShowS
catShows = foldr (.) id

-- | Show a list by applying a function to each element
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith f = catShows . map f

-- | Show a list by applying a function to each element, and putting a separator between them
showListWithSep :: String -> (a -> ShowS) -> [a] -> ShowS
showListWithSep sep f = catShows . intersperse (showString sep) . map f

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: StdGen -> [a] -> [a]
shuffle' gen xs = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        --gen' <- readSTRef g
        return xs')
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray nn xxs = newListArray (1,nn) xxs

--------------------------------------------------------------------------------
-- Randomness
--------------------------------------------------------------------------------

-- Return a new random value
getRandomR :: (Random a, MonadState StdGen m) => (a,a) -> m a
getRandomR r = do
    s <- get
    let (a,s') = randomR r s
    put s'
    return a

-- Return a new random value
getRandomRs :: (Random a, MonadState StdGen m) => (a,a) -> m [a]
getRandomRs r = do
    g <- getGen
    return $ randomRs r g

-- Return a new random value
getGen :: MonadState StdGen m => m StdGen
getGen = do
    s <- get
    let (a,b) = split s
    put b
    return a

type Rand = State StdGen

instance Applicative (State s) where
    pure = return
    (<*>) = ap

runRandom :: Int -> Rand a -> a
runRandom seed x = evalState x (mkStdGen seed)


instance (Random a, Random b) => Random (a,b) where
    random g = let (a,g' ) = random g
                   (b,g'') = random g'
               in ((a,b),g'')
    randomR ((u,v),(w,x)) g =
               let (a,g' ) = randomR (u,w) g
                   (b,g'') = randomR (v,x) g'
               in ((a,b),g'')


rotate :: Int -> [a] -> [a]
rotate n xs = rest ++ begin
  where (begin,rest) = splitAt n xs

-- | Randomly rotate
randomRot :: [a] -> Rand [a]
randomRot xs = do
     steps <- getRandomR (0,length xs - 1)
     return $ rotate steps xs

-- | Randomly flip
randomFlip :: [a] -> Rand [a]
randomFlip xs = do
     rev <- getRandomR (False,True)
     return $ if rev then reverse xs else xs

-- | Randomly take at most n elements
randomTake :: Int -> [a] -> Rand [a]
randomTake n xs | length xs <= n = return xs
randomTake n xs = do
       chosen <- shuffle $ replicate n return ++ replicate (length xs - n) (const [])
       return $ concat $ zipWith ($) chosen xs

shuffle :: [a] -> Rand [a]
shuffle xs = flip shuffle' xs <$> getGen

-----------------------------------------------------------------------------
-- Inputs/problem sets
-----------------------------------------------------------------------------

type Point = (Int,Int)

data Run = Run [Point]
  deriving (Eq)

showPoint :: Point -> ShowS
showPoint (x,y)
  | abs x >= 10^6 || abs y >= 10^6 = error "point too large"
  | otherwise = shows x . showString " " . shows y

instance Show Run where
    showsPrec _ (Run ps)
      | length ps >= 10^6 = error "too many points"
      | otherwise = shows (length ps) . showString "\n" . showListWithSep "\n" showPoint ps

-----------------------------------------------------------------------------
-- Simple edge cases
-----------------------------------------------------------------------------

-- box with steps 1 -> dead
mkBox :: Int -> [Point]
mkBox n = concat [ [(x,n),(x,-n),(n,x),(-n,x)] | x <- [-n..n-1] ]

simple :: [Run]
simple =
    [ Run $ shuffle' (mkStdGen seed) $ mkBox i
    | (i,seed) <- zip [3..50] [0x4351345..]
    ]
 ++ [ Run $ shuffle' (mkStdGen (123*seed)) $ tail $ shuffle' (mkStdGen seed) $ mkBox i
    | (i,seed) <- zip [3..50] [0x66666645,0x453414..]
    ]

-----------------------------------------------------------------------------
-- Large random things
-----------------------------------------------------------------------------

randomPoint :: Int -> Rand Point
randomPoint r = do
    p@(x,y) <- getRandomR ((-r,-r),(r,r))
    if x^2+y^2 <= 2^2
      then randomPoint r -- try again
      else return p

randomRun :: Int -> Int -> Rand Run
randomRun r n = do
    ps <- replicateM n (randomPoint r)
    ps' <- shuffle ps
    return $ Run ps'

randomRuns =
     [ runRandom seed $ randomRun 20 10
     | seed <- take 20 [0xFA4541541..]
     ]
  ++ [ runRandom seed $ randomRun 20 30
     | seed <- take 20 [0x652642..]
     ]
  ++ [ runRandom seed $ randomRun 20 50
     | seed <- take 20 [0x43643165..]
     ]
  ++ [ runRandom seed $ randomRun 20 80
     | seed <- take 20 [0x85475334..]
     ]
  ++ [ runRandom seed $ randomRun 20 100
     | seed <- take 20 [0x54513513..]
     ]
  ++ [ runRandom seed $ randomRun 20 200
     | seed <- take 20 [0x6562456..]
     ]
  ++ [ runRandom seed $ randomRun 500 1000
     | seed <- take 20 [0x98762549..]
     ]
  ++ [ runRandom seed $ randomRun 500 4000
     | seed <- take 20 [0x54513536..]
     ]
  ++ [ runRandom seed $ randomRun 2000 10000
     | seed <- take 10 [0x7FEE3514..]
     ]
{-
   ++[ runRandom seed $ mkRadialRun 1000 20
     | seed <- take 20 [0x453151..]
     ]
-}

-----------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------

main = do
    print (length runs)
    mapM_ print runs
  where
    runs = simple ++ randomRuns


-----------------------------------------------------------------------------
-- debugging
-----------------------------------------------------------------------------

svgCode (Run ps) = unlines $
    ["<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>"
    ,"<svg viewBox='" ++ showListWithSep " " shows bounds "' width='100%' height='100%' version='1.1' xmlns='http://www.w3.org/2000/svg'>"
    ," <style>"
    ,"   * { "
    ,"     fill: red;"
    ,"     stroke: black;"
    ,"     stroke-width: 0.05;"
    ,"   }"
    ,"   .me {"
    ,"     fill: green;"
    ,"   }"
    ," </style>"
    ," <rect x='-0.5' y='-0.5' width='1' height='1' class='me'/>"
    ]
  ++[" <circle cx='"++show x++"' cy='"++show y++"' r='2' />"
    | (x,y) <- ps
    ]
  ++["</svg>"
    ]
  where bounds = [minimum xs - 2, minimum ys - 2, maximum xs - minimum xs + 4, maximum ys - minimum ys + 4]
        (xs,ys) = unzip ps

badrun1 = (simple++randomRuns) !! 54
