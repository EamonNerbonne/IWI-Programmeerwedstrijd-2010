import InputValidation

instruction = do
    w <- word
    case w of
       "istore" -> word >> return ()
       "iload"  -> word >> return ()
       "iconst" -> nonNegativeInteger >> return ()
       "iadd" -> return ()
       "isub" -> return ()
       "imul" -> return ()
       "idiv" -> return ()
       _ -> fail "invalid instruction"

run = do n <- lineWith (positiveInteger .<. 10^6)
         n `times_` lineWith instruction

main = validateIO $ runsOf run
