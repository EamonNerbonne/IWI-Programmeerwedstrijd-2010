import InputValidation

run = do n <- lineWith (positiveInteger .<. 10^6)
         lineWith $ do
             xs <- timesSepBy n space (nonNegativeInteger .<. 10^6)
             unless (sum xs < 10^9) (fail $ "sum doesn't fit in a signed int")
             unless (sum xs `mod` n == 0) (fail $ "sum of stacks not evenly divisible by n")

main = validateIO $ runsOf run
