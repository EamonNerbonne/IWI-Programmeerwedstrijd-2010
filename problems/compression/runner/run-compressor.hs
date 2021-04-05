{-# LANGUAGE BangPatterns #-}
import System.Process
import System.IO
import System.Exit
import System.Environment
import System.Directory
import System.Console.GetOpt
import Data.Char
import Data.Maybe
import Control.Monad

-------------------
-- Input / code text

isValidCodeChar :: Char -> Bool
isValidCodeChar x = x == ' ' || x == '.' || (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || (x >= '0' && x <= '9')

isValidTextChar :: Char -> Bool
--isValidTextChar x = x == ' ' || (x >= 'a' && x <= 'z')
isValidTextChar = isValidCodeChar


-- | Valid chars, returns first failure
invalidTextChars :: String -> [Char]
invalidTextChars = catMaybes . map (\x -> if isValidTextChar x then Nothing else Just x)

-- | Valid chars, returns first failure
invalidCodeChars :: String -> [Char]
invalidCodeChars = catMaybes . map (\x -> if isValidCodeChar x then Nothing else Just x)

-- | Filter input text
filterText :: String -> String
filterText = go False
  where go _  [] = []
        go sp (x:xs)
          | isValidTextChar x = (if sp && x /= ' ' then (' ':) else id) $ x : go False xs
          | x == '\'' = go sp xs -- ignore completely
          | otherwise = go True xs -- combine spaces

-------------------
-- Comparison

-- for output
limit :: Int -> String -> String
limit _ [] = []
limit 0 _  = "..."
limit i (x:xs) = x : limit (i-1) xs

showLimit = show . limit 20 . dropWhile (=='@')

compareOutput :: String -> String -> (Bool, String)
compareOutput xs ys = go 0 xs ys (replicate 10 '@' ++ xs) (replicate 10 '@' ++ ys)
  where 
    go _ [] [] _ _ = (True, "ok")
    go !i (x:xs) (y:ys) (_:us) (_:vs) | x == y = go (i+1) xs ys us vs
    go i _ _ xs ys = (False, "Wrong output at position " ++ show i ++ ": "
                             ++ showLimit xs ++ " should be "
                             ++ showLimit ys)

-------------------
-- Running the programs

die :: String -> IO a
die x = do
    hPutStrLn stderr x
    exitFailure

runMyCommand :: [FilePath] -> IO (Handle,Handle,ProcessHandle)
runMyCommand (program:args) = do
    (mb_in, mb_out, _, p) <- createProcess (proc program args)
                                               { std_in  = CreatePipe
                                               , std_out = CreatePipe
                                               , std_err = Inherit } 
    let h_in = fromJust mb_in; h_out = fromJust mb_out
    hSetBinaryMode h_in False
    hSetBinaryMode h_out False
    return (h_in,h_out,p)

dieOnFailure :: String -> ExitCode -> IO ()
dieOnFailure msg ExitSuccess = return ()
dieOnFailure msg e = do
    hPutStrLn stderr msg
    exitWith e

-- | Test a string
runTests :: Flags -> [FilePath] -> String -> IO Int
runTests flags program text = do
    -- the test case
    case invalidTextChars text of
       [] -> return ()
       xs -> die $ "Input text contains invalid characters:\n" ++ limit 10 xs -- shouldn't happen after filter
    -- print input
    when (flagVerbose flags) $ do
        putStrLn "Plaintext:"
        putStrLn text
        putStrLn ""
    
    -- encode
    (e_in,e_out,e_process) <- runMyCommand program
    hPutStrLn e_in "compress"
    hPutStrLn e_in text
    hFlush e_in
    coded <- hGetLine e_out
    case invalidCodeChars coded of
       [] -> return ()
       xs -> die $ "Compressed text contains invalid characters:\n" ++ limit 10 xs
    -- close
    hClose e_in
    hClose e_out
    waitForProcess e_process >>= dieOnFailure "Compressor ended abnormally"
    -- print encoded
    when (flagVerbose flags) $ do
        putStrLn "Compressed:"
        putStrLn coded
        putStrLn ""
        
    -- decode
    (d_in,d_out,d_process) <- runMyCommand program
    hPutStrLn d_in "decompress"
    hPutStrLn d_in coded
    hFlush d_in
    decoded <- hGetLine d_out
    -- close
    hClose d_in
    hClose d_out
    waitForProcess d_process >>= dieOnFailure "Decompressor ended abnormally"
    
    -- compare
    let (same,message) = compareOutput decoded text
    when (flagVerbose flags && not same) $ do
        putStrLn "Decompressed:"
        putStrLn decoded
        putStrLn ""
    putStrLn $ message
    
    -- report length
    return $! length coded

{-
-- | Test a string
runTest :: FilePath -> String -> IO Int
runTest program text = do
    -- the test case
    unless (isValidText text) $ die "Input text contains invalid characters" -- shouldn't happen after filter
    -- encode
    coded <- readProcess program [] ("encoder\n"++text)
    unless (isValidCode coded) $ die "Encoded text contains invalid characters" -- todo: provide more helpful information
    -- decode
    decoded <- readProcess program [] ("decoder\n"++text)
    -- compare
    putStrLn $ if text == decoded then "ok" else "differ"
    -- report length
    return $! length coded
-}

data Flags = Flags { flagVerbose :: Bool }
defaultFlags :: Flags
defaultFlags = Flags { flagVerbose = False }

main :: IO ()
main = do
   -- parse command line
   args <- getArgs
   case getOpt Permute options args of
      (_,[],[]) -> usage
      (f,xs,[]) -> run (foldl (flip id) defaultFlags f) xs
      (_,_,err) | not (null err) -> die $ unlines err
 where
   options = [Option "v" ["verbose"] (NoArg $ \flags -> flags { flagVerbose = True }) "print input and output of the compressor"
             ,Option "?" ["help"]    (NoArg id) "show this help screen"]
   usage = do self <- getProgName 
              let helpString = unlines
                     ["Usage: " ++ self ++ " [OPTIONS] ./compressor-executable < testcase"
                     ,""
                     ,"Test the 'compressor-executable' program, see the problem text for details."
                     ,""
                     ,"Options: "]
              die $ usageInfo helpString options


run :: Flags -> [FilePath] -> IO ()
run flags program = do
   inputStr <- getContents
   let inputs = filter (not.null) . map filterText . lines $ inputStr
   let inSizes = map length $ inputs
   sizes <- mapM (runTests flags program) inputs
   writeCompressionResult (length inputs) (sum inSizes) (sum sizes)

writeCompressionResult numCases sizeIn sizeOut = do
   magicOut <- doesFileExist "program.magic"
   when magicOut $ do
       -- output to magic file,
       writeFile "program.magic" $ show (fromIntegral sizeOut / fromIntegral sizeIn)
   when (sizeOut >= sizeIn) $ do
       hPutStrLn stdout "This is not actually compression."
   -- output to stderr for normal usage 
   hPutStrLn stderr $ "#testcases: " ++ show numCases
   hPutStrLn stderr $ "input:      " ++ show sizeIn ++ " chars"
   hPutStrLn stderr $ "compressed: " ++ show sizeOut ++ " chars"
   hPutStrLn stderr $ "compression ratio: " ++ show (fromIntegral sizeOut / fromIntegral sizeIn)
