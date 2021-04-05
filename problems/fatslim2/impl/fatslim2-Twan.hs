import Control.Monad
import Control.Applicative
import Data.List

-- 16:45 - 17:05 (inclusief tekst herschrijven)

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

type Person = (Int,Int) -- arrival, ticket machine duration

solve :: Int -> Int -> [Person] -> Int
solve n_g n_t = count (<= 4 * 60 * 60) . accessGates n_g . ticketMachines n_t

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

buyTicket :: Person -> (Int, Maybe (Int -> Int))
buyTicket (t,0)  = (t, Nothing)
buyTicket (t,dt) = (t, Just (+dt))

passGate :: Int -> (Int, Maybe (Int -> Int))
passGate t = (t, Just (+1))

ticketMachines :: Int -> [Person] -> [Int]
ticketMachines n = sort . simulateQueues buyTicket (replicate n 0)

accessGates :: Int -> [Int] -> [Int]
accessGates n = simulateQueues passGate (replicate n 0)

simulateQueues :: Ord a => (b -> (a, Maybe (a -> a))) -> [a] -> [b] -> [a]
simulateQueues _  _          []         = []
simulateQueues dt qqs@(q:qs) (x:xs) = case dt x of
     (a, Nothing) -> a : simulateQueues dt qqs xs
     (a, Just da) -> let a' = da (max a q)
                     in  a' : simulateQueues dt (insert a' qs) xs

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

readPerson :: String -> Person
readPerson xs = case map read $ words xs of [a,b] -> (a,b)

run = do
    [n_g,n_t] <- map read . words <$> getLine
    n <- readLn
    persons <- replicateM n (readPerson `fmap` getLine)
    print (n - solve n_g n_t persons)

main = flip replicateM_ run =<< readLn
