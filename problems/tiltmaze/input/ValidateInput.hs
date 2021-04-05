import InputValidation

run = do (h,w) <- lineWith (positiveInteger .<. 10^3,positiveInteger .<. 10^3)
         h `times_` lineWith (w `times_` (char `with` (`elem` "AB.X")))

main = validateIO $ runsOf run
