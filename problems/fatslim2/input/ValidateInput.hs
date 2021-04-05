import InputValidation

run = do lineWith $ (positiveInteger .<. 10, positiveInteger .<. 10)
         n <- lineWith (nonNegativeInteger .<. 10^6)
         ps <- n `times` lineWith (nonNegativeInteger .<. 10^9  -- t
                                  ,nonNegativeInteger .<=. 30*60) -- d
         myGuard "sorted by arrival time" (isSorted $ map fst ps)

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
isSorted _        = True

main = validateIO $ runsOf run
