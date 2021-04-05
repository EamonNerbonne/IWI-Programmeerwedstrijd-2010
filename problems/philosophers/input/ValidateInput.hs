import InputValidation

run = lineWith (positiveInteger .<. 10^6)
main = validateIO $ runsOf run
