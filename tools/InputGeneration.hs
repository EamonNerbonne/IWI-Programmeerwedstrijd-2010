{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
module InputGeneration
    ( module InputGeneration
    , module Control.Monad
    , module Control.Applicative
    , module System.Random
    , module Data.Maybe
    , module Data.List
    ) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Applicative
import System.IO
import System.Random
import Data.Array.ST hiding (range,newArray)
import Data.STRef
import Control.Monad
import Control.Monad.ST
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Utilities
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

-- | Show a list by applying a function to each element, and putting a separator between them and at the end
showListWithEnd :: String -> (a -> ShowS) -> [a] -> ShowS
showListWithEnd end f = catShows . map (\x -> f x . showString end)

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort

fastNub :: Ord a => [a] -> [a]
fastNub = go Set.empty
  where go _ [] = []
        go seen (x:xs)
          | x `Set.member` seen = go seen xs
          | otherwise           = x : go (Set.insert x seen) xs

-- From Data.List
permutations'            :: [a] -> [[a]]
permutations' xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations' is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
subsequences'            :: [a] -> [[a]]
subsequences' xs         =  [] : nonEmptySubsequences' xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences'         :: [a] -> [[a]]
nonEmptySubsequences' []      =  []
nonEmptySubsequences' (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences' xs)
  where f ys r = ys : (x : ys) : r


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

-- Return many new random values
getRandomRs :: (Random a, MonadState StdGen m) => (a,a) -> m [a]
getRandomRs r = do
    g <- getGen
    return $ randomRs r g

-- Return a new random generator
getGen :: MonadState StdGen m => m StdGen
getGen = do
    s <- get
    let (a,b) = split s
    put b
    return a

-- Random shuffle
shuffle :: [a] -> Rand [a]
shuffle xs = flip shuffle' xs <$> getGen

randomChoice :: [a] -> Rand a
randomChoice xs | null xs = error "randomChoice: empty list"
randomChoice xs = do
    i <- getRandomR (0,length xs-1)
    return (xs!!i)


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

--------------------------------------------------------------------------------
-- Random shuffle
--------------------------------------------------------------------------------

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
-- Wrappers
--------------------------------------------------------------------------------

printRuns :: Show a => [a] -> IO ()
printRuns xs = do
    hSetBinaryMode stdout True -- die windows, die
    print (length xs)
    mapM_ print xs

printRunsNoBr :: Show a => [a] -> IO ()
printRunsNoBr xs = do
    hSetBinaryMode stdout True -- die windows, die
    print (length xs)
    mapM_ (putStr . show) xs
