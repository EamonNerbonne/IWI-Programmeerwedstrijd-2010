import InputValidation

run = do lineWith $ do s <- positiveInteger .<. 10^9
                       space
                       w <- positiveInteger .>=. s .<. 10^9
                       return (s,w)
         n <- lineWith (nonNegativeInteger .<. 10^6)
         n `times_` lineWith (nonNegativeInteger .<. 10^9 -- t
                             ,nonNegativeInteger .<. 10^9 -- d
                             ,word `with` (`elem`["L","XL"])) -- fat?

main = validateIO $ runsOf run
