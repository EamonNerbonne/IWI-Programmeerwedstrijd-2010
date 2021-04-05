{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- | Efficient combinatorial algorithms to generate all permutations
--   and partitions of a multiset.  Note that an 'Eq' or 'Ord'
--   instance on the elements is /not/ required; the algorithms are
--   careful to keep track of which things are (by construction) equal
--   to which other things, so equality testing is not needed.
module MathCombinatoricsMultiSet
       ( -- * The 'MultiSet' type

         Count
       , MultiSet
       , toList
       , fromList

         -- * Permutations

       , permutations
       , permutationsRLE

         -- * Partitions

       , Vec
       , vPartitions
       , partitions

       ) where

import Data.List (group, sort)
import Control.Arrow (first, second, (&&&))
import Data.Maybe (catMaybes)

type Count = Int

-- | A multiset is a list of (element, count) pairs.  We maintain the
--   invariants that the counts are always positive, and no element
--   ever appears more than once.
type MultiSet a = [(a, Count)]

-- | Convert a multiset to a list.
toList :: MultiSet a -> [a]
toList = concatMap (uncurry (flip replicate))

-- | Convert a list to a multiset.  This method is provided just for
--   convenience; you can of course construct your own 'MultiSet's
--   directly (especially if the type of the elements is not an
--   instance of 'Ord').
fromList :: Ord a => [a] -> MultiSet a
fromList = map (head &&& length) . group . sort

-- | In order to generate permutations of a multiset, we need to keep
--   track of the most recently used element in the permutation being
--   built, so that we don't use it again immediately.  The
--   'RMultiSet' type (for \"restricted multiset\") records this
--   information, consisting of a multiset possibly paired with an
--   element (with multiplicity) which is also part of the multiset,
--   but should not be used at the beginning of permutations.
data RMultiSet a = RMS (Maybe (a, Count)) (MultiSet a)
  deriving Show

-- | Convert a 'MultiSet' to a 'RMultiSet' (with no avoided element).
toRMS :: MultiSet a -> RMultiSet a
toRMS = RMS Nothing

-- | Convert a 'RMultiSet' to a 'MultiSet'.
fromRMS :: RMultiSet a -> MultiSet a
fromRMS (RMS Nothing m)  = m
fromRMS (RMS (Just e) m) = e:m

-- | List all the distinct permutations of the elements of a
--   multiset.
--
--   For example, @permutations [('a',1), ('b',2)] ==
--   [\"abb\",\"bba\",\"bab\"]@, whereas @Data.List.permutations
--   \"abb\" == [\"abb\",\"bab\",\"bba\",\"bba\",\"bab\",\"abb\"]@.
--   This function is equivalent to, but /much/ more efficient than,
--   @nub . Data.List.permutations@, and even works when the elements
--   have no 'Eq' instance.
--
--   Note that this is a specialized version of 'permutationsRLE',
--   where each run has been expanded via 'replicate'.
permutations :: MultiSet a -> [[a]]
permutations = map toList . permutationsRLE

-- | List all the distinct permutations of the elements of a multiset,
--   with each permutation run-length encoded. (Note that the
--   run-length encoding is a natural byproduct of the algorithm used,
--   not a separate postprocessing step.)
--
--   For example, @permutationsRLE [('a',1), ('b',2)] ==
--   [[('a',1),('b',2)],[('b',2),('a',1)],[('b',1),('a',1),('b',1)]]@.
--
--   (Note that although the output type is equivalent to @[MultiSet
--   a]@, we don't call it that since the output may violate the
--   'MultiSet' invariant that no element should appear more than
--   once.  And indeed, morally this function does not output
--   multisets at all.)
permutationsRLE :: MultiSet a -> [[(a,Count)]]
permutationsRLE [] = [[]]
permutationsRLE m  = permutationsRLE' (toRMS m)

-- | List all the (run-length encoded) distinct permutations of the
-- elements of a multiset which do not start with the element to avoid
-- (if any).
permutationsRLE' :: RMultiSet a -> [[(a,Count)]]

-- If only one element is left, there's only one permutation.
permutationsRLE' (RMS Nothing [(x,n)]) = [[(x,n)]]

-- Otherwise, select an element+multiplicity in all possible ways, and
-- concatenate the elements to all possible permutations of the
-- remaining multiset.
permutationsRLE' m  = [ e : p
                      | (e, m') <- selectRMS m
                      , p       <- permutationsRLE' m'
                      ]

-- | Select an element + multiplicity from a multiset in all possible
--   ways, appropriately keeping track of elements to avoid at the
--   start of permutations.
selectRMS :: RMultiSet a -> [((a, Count), RMultiSet a)]

-- No elements to select.
selectRMS (RMS _ [])            = []

-- Selecting from a multiset with n copies of x, avoiding e:
selectRMS (RMS e ((x,n) : ms))  =

  -- If we select all n copies of x, there are no copies of x left to avoid;
  -- stick e (if it exists) back into the remaining multiset.
  ((x,n), RMS Nothing (maybe ms (:ms) e)) :

  -- We can also select any number of copies of x from (n-1) down to 1; in each case,
  -- we avoid the remaining copies of x and put e back into the returned multiset.
  [ ( (x,k), RMS (Just (x,n-k))
                 (maybe ms (:ms) e) )
    | k <- [n-1, n-2 .. 1]
  ] ++

  -- Finally, we can recursively choose something other than x.
  map (second (consRMS (x,n))) (selectRMS (RMS e ms))

consRMS :: (a, Count) -> RMultiSet a -> RMultiSet a
consRMS x (RMS e m) = RMS e (x:m)


-- Some QuickCheck properties.  Of course, due to combinatorial
-- explosion these are of limited utility!
-- newtype ArbCount = ArbCount Int
--   deriving (Eq, Show, Num, Real, Enum, Ord, Integral)

-- instance Arbitrary Count where
--   arbitrary = elements (map ArbCount [1..3])

-- prop_perms_distinct :: MultiSet Char ArbCount -> Bool
-- prop_perms_distinct m = length ps == length (nub ps)
--   where ps = permutations m

-- prop_perms_are_perms :: MultiSet Char ArbCount -> Bool
-- prop_perms_are_perms m = all ((==l') . sort) (permutations m)
--   where l' = sort (toList m)

---------------------
-- Partitions
---------------------

-- | Element count vector.
type Vec = [Count]

-- | Componentwise comparison of count vectors.
(<|=) :: Vec -> Vec -> Bool
xs <|= ys = and $ zipWith (<=) xs ys

-- | 'vZero v' produces a zero vector of the same length as @v@.
vZero :: Vec -> Vec
vZero = map (const 0)

-- | Test for the zero vector.
vIsZero :: Vec -> Bool
vIsZero = all (==0)

-- | Do vector arithmetic componentwise.
(.+.), (.-.) :: Vec -> Vec -> Vec
(.+.) = zipWith (+)
(.-.) = zipWith (-)

-- | Multiply a count vector by a scalar.
(*.) :: Count -> Vec -> Vec
(*.) n = map (n*)

-- | 'v1 `vDiv` v2' is the largest scalar multiple of 'v2' which is
--   elementwise less than or equal to 'v1'.
vDiv :: Vec -> Vec -> Count
vDiv v1 v2 = minimum . catMaybes $ zipWith zdiv v1 v2
  where zdiv _ 0 = Nothing
        zdiv x y = Just $ x `div` y

-- | 'vInc within v' lexicographically increments 'v' with respect to
--   'within'.  For example, @vInc [2,3,5] [1,3,4] == [1,3,5]@, and
--   @vInc [2,3,5] [1,3,5] == [2,0,0]@.
vInc :: Vec -> Vec -> Vec
vInc lim v = reverse (vInc' (reverse lim) (reverse v))
  where vInc' _ []          = []
        vInc' [] (x:xs)     = x+1 : xs
        vInc' (l:ls) (x:xs) | x < l     = x+1 : xs
                            | otherwise = 0 : vInc' ls xs

-- | Generate all vector partitions, representing each partition as a
--   multiset of vectors.
--
--   This code is a slight generalization of the code published in
--
--     Brent Yorgey. \"Generating Multiset Partitions\". In: The
--     Monad.Reader, Issue 8, September 2007.
--     <http://www.haskell.org/sitewiki/images/d/dd/TMR-Issue8.pdf>
--
--   See that article for a detailed discussion of the code and how it works.
vPartitions :: Vec -> [MultiSet (Vec)]
vPartitions v = vPart v (vZero v) where
  vPart v _ | vIsZero v = [[]]
  vPart v vL
    | v <= vL   = []
    | otherwise = [(v,1)] : [ (v',k) : p' | v' <- withinFromTo v (vHalf v) (vInc v vL)
                                          , k  <- [1 .. (v `vDiv` v')]
                                          , p' <- vPart (v .-. (k *. v')) v' ]

-- | 'vHalf v' computes the \"lexicographic half\" of 'v', that is,
--   the vector which is the middle element (biased towards the end)
--   in a lexicographically decreasing list of all the vectors
--   elementwise no greater than 'v'.
vHalf :: Vec -> Vec
vHalf [] = []
vHalf (x:xs) | (even x) = (x `div` 2) : vHalf xs
             | otherwise = (x `div` 2) : xs

downFrom n = [n,(n-1)..0]

-- | 'within m' generates a lexicographically decreasing list of
--   vectors elementwise no greater than 'm'.
within :: Vec -> [Vec]
within = sequence . map downFrom

-- | Clip one vector against another.
clip :: Vec -> Vec -> Vec
clip = zipWith min

-- | 'withinFromTo m s e' efficiently generates a lexicographically
--   decreasing list of vectors which are elementwise no greater than
--   'm' and lexicographically between 's' and 'e'.
withinFromTo :: Vec -> Vec -> Vec -> [Vec]
withinFromTo m s e | not (s <|= m) = withinFromTo m (clip m s) e
withinFromTo m s e | e > s = []
withinFromTo m s e = wFT m s e True True
  where
    wFT [] _ _ _ _ = [[]]
    wFT (m:ms) (s:ss) (e:es) useS useE =
        let start = if useS then s else m
            end   = if useE then e else 0
        in
          [x:xs | x <- [start,(start-1)..end],
                  let useS' = useS && x==s,
                  let useE' = useE && x==e,
                  xs <- wFT ms ss es useS' useE' ]

-- | Efficiently generate all distinct multiset partitions.  Note that
--   each partition is represented as a multiset of parts (each of
--   which is a multiset) in order to properly reflect the fact that
--   some parts may occur multiple times.
partitions :: MultiSet a -> [MultiSet (MultiSet a)]
partitions [] = [[]]
partitions m  = (map . map . first) (combine elts) $ vPartitions counts
  where (elts, counts) = unzip m
        combine es cs  = filter ((/=0) . snd) $ zip es cs
