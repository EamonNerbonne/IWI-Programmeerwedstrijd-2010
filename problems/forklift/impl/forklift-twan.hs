
-- 12:25 - 13:05

import Control.Monad

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

afoldl :: (a -> b -> a) -> (a -> a) -> a -> [b] -> a
afoldl f i z _ | z `seq` False = undefined
afoldl f i z []     = z
afoldl f i z [x]    = f z x
afoldl f i z (x:xs) = afoldl f i (i (f z x)) xs

ascanl :: (a -> b -> a) -> (a -> a) -> a -> [b] -> [a]
ascanl f i z _ | z `seq` False = undefined
ascanl f i z []     = [z]
ascanl f i z [x]    = [f z x]
ascanl f i z (x:xs) = f z x : i (f z x) : ascanl f i (i (f z x)) xs

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

-- keep the two best solutions:
--   sol1 = length of solution ending at the end
--   sol2 = length of solution that then needs to drive back by back2
-- invariant:
--   sol2+back2 <= sol1
data State = S { pos :: !Int, sol1, sol2 :: !Int, back2 :: !Int, carry :: !Int } deriving Show

start :: State
start = S 0 0 0 0 0

solve :: [Int] -> Int
solve = solve' . makeZeroMean

solve' :: [Int] -> Int
solve' = solution . afoldl step move start . dropTailWhile (==0)

solution :: State -> Int
solution s = sol2 s + back2 s

step :: State -> Int -> State
step (S p s1 s2 b2 c) x = S p s1 s2 b2 (c+x)

move :: State -> State
move (S p s1 s2 b2 c)
  | s1' <= s2' + b2' = S p' s1' s1' 0   c
  | otherwise        = S p' s1' s2' b2' c
  where p' = p + 1
        s1' | c >= 0    = s1 + 1
            | otherwise = s1 + 3 -- need to return here later
        s2' = s2 + 1
        b2' | b2 == 0 && c >= 0 = 0
            | otherwise         = b2 + 1

makeZeroMean xs = map (subtract (sum xs `div` length xs)) xs

dropTailWhile p = reverse . dropWhile p . reverse

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

run = do
    _ <- getLine -- length of next line
    x <- getLine
    let input = map read $ words x
    print $ solve input * 2

main = flip replicateM_ run =<< readLn
