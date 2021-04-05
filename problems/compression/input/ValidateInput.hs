import InputValidation

run = do s <- entireLine
         (return (length s) `named` ("line length "++)) .<. 10^6

main = validateIO $ allLines run
