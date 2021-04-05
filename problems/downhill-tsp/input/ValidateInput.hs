import InputValidation

run = do n <- lineWith (nonNegativeInteger .<. 10^6)
         n `times_` lineWith (positiveInteger .<. 10^9, positiveInteger .<. 10^9)

main = validateIO $ runsOf run
