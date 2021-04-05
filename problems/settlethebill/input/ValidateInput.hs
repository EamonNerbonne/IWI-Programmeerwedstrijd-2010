import InputValidation

run = do (n,m) <- lineWith $ (positiveInteger .<. 20, positiveInteger .<. 400)
         m `times_` lineWith (nonNegativeInteger .<. n  -- f
                             ,nonNegativeInteger .<. n  -- t
                             ,nonNegativeInteger .<. 10^7) -- a

main = validateIO $ runsOf run
