
import Data.Char
import Data.List
import Data.Ord

from64 :: Char -> Int
from64 x
  | x >= 'a' && x <= 'z' = ord x - ord 'a'
  | x >= 'A' && x <= 'Z' = ord x - ord 'A' + 26
  | x >= '0' && x <= '9' = ord x - ord '0' + 26 + 26
  | x == ' '             = 26 + 26 + 10
  | x == '.'             = 26 + 26 + 10 + 1
  | otherwise            = error "invalid base64 char"

to64 :: Int -> Char
to64 x
  | x      < 26           = chr $ x + ord 'a'
  | x - 26 < 26           = chr $ x + ord 'A' - 26
  | x - 26 - 26 < 10      = chr $ x + ord '0' - 26 - 26
  | x - 26 - 26 - 10 == 0 = ' '
  | x - 26 - 26 - 10 == 1 = '.'
  | otherwise            = error "can't encode"


-- at most 64 entries!
table :: [String]
table = [ [x]     | x <- ['a'..'z']++[' '] ]
     ++ [ [' ',x] | x <- ['a'..'z']++[' '] ]
     ++ [" to","ing","is","th","nd","et","ion","he","es","as"]

findTable :: String -> Maybe (Int,String)
findTable "" = Nothing
findTable xs = Just $ bestMatch
  where matches = tableMatches xs
        bestMatch = snd $ maximumBy (comparing fst) matches

tableMatches :: String -> [(Int,(Int,String))]
tableMatches xs = [ (l,(i,drop l xs)) | (i,u) <- zip [0..] table, u `isPrefixOf` xs, let l = length u ]



compress, decompress :: String -> String

-- Compression using table
compress = map to64 . unfoldr findTable


-- De compression
decompress = concatMap ((table!!) . from64)



main = do
    mode <- getLine
    input <- getLine
    putStrLn $ case mode of
       "compress" -> compress input
       "decompress" -> decompress input
       _            -> error "unknown mode"


test = "alice was beginning to get very tired of sitting by her sister on the bank and of having nothing to do once or twice she had peeped into the book her sister was reading but it had no pictures or conversations in it and what is the use of a book thought alice without pictures or conversation"
