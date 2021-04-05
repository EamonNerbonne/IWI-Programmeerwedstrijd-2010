{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module InputValidation
    ( Error(..)
    , MonadValidate(..), myGuard
    , (.>.),(.>=.),(.<.),(.<=.),(.==.)
    
    , word, char
    , integer, positiveInteger, nonNegativeInteger
    , space, entireLine
    
    , validate
    , validateIO
    
    , lineWith, allLines
    , times, times_, timesSepBy
    , runsOf
    
    , module Control.Monad
    , module Control.Applicative
    ) where

import Control.Monad
import Control.Applicative
import System.IO
import System.Exit
import Data.Char

--------------------------------------------------------------------------------
-- Generic validation monad
--------------------------------------------------------------------------------

data Error = Error { eWhere, eExpect, eFound :: String }
newtype Validate s a = V { unV :: s -> Either Error (a,s) }

instance Show Error where
    show (Error w ""  found) = w ++ "Unexpected " ++ found
    show (Error w exp "")    = w ++ "Expected " ++ exp
    show (Error w exp found) = w ++ "Expected " ++ exp ++ " instead of " ++ found

class Monad m => MonadValidate m where
    with :: Show a => m a -> (a -> Bool) -> m a
    onError :: m a -> (Error -> Error) -> m a
    named :: m a -> (String -> String) -> m a
    a `named` n = a `onError` (\(Error w e f) -> Error w (n e) f)
    (<?>) :: m a -> String -> m a
    a <?> n = a `named` (const n)

instance Functor (Validate s) where
    fmap f x = V $ \s -> case unV x s of
                             Left e -> Left e
                             Right (a,s') -> Right (f a,s')

instance Applicative (Validate s) where
    pure = return
    (<*>) = ap

instance Monad (Validate s) where
    return a  = V $ \s -> Right (a,s)
    aa >>= m = V $ \s -> case unV aa s of
                             Left e -> Left e
                             Right (a,s') -> unV (m a) s'
    fail e = V $ \_ -> Left (Error "" "" e)

instance MonadValidate (Validate s) where
    with x test = do a <- x
                     unless (test a) (fail $ "invalid value: " ++ show a)
                     return a
    onError aa name = V $ \s -> case unV aa s of
                                  Left e -> Left (name e)
                                  Right r -> Right r

myGuard :: MonadValidate m => String -> Bool -> m ()
myGuard _   True  = return()
myGuard msg False = fail msg

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

(.>.),(.>=.),(.<.),(.<=.),(.==.) :: (Show a, Ord a, MonadValidate m) => m a -> a -> m a
a .>.  b = a `with` (>b)  `named` (++(" > "  ++ show b))
a .<.  b = a `with` (<b)  `named` (++(" < "  ++ show b))
a .>=. b = a `with` (>=b) `named` (++(" >= " ++ show b))
a .<=. b = a `with` (<=b) `named` (++(" <= " ++ show b))
a .==. b = a `with` (==b) `named` (const (show b))

infixl 2 <?>, `named`
infixl 5 .>., .>=., .<., .<=., .==., `with`

--------------------------------------------------------------------------------
-- Things on lines
--------------------------------------------------------------------------------

newtype InLine a = I (Validate String a) deriving (Functor,Applicative,Monad,MonadValidate)

-- | A word ended by a space or newline
wordOf :: (Char -> Bool) -> (Char -> Bool) -> InLine String
wordOf first next = I . V $ parseWord1
  where parseWord1 []               = Left $ Error "" "word" "end of line"
        parseWord1 (x:xs) | first x = (x:) <$$$> parseWord2 xs
        parseWord1 (x:_)            = Left $ Error "" "word" ("invalid char: "++show x)
        parseWord2 xs@[]            = Right ("",xs)
        parseWord2 xs@(' ':_)       = Right ("",xs)
        parseWord2 (x:xs) | next x  = (x:) <$$$> parseWord2 xs
        parseWord2 (x:_)            = Left $ Error "" "word" ("invalid char: "++show x)
        f <$$$> Left e = Left e
        f <$$$> Right (a,b) = Right (f a,b)

-- | A word ended by a space or newline
word :: InLine String
word = wordOf (not . isSpace) (const True)

-- | Any character
char :: InLine Char
char = I . V $ \s -> case s of
                      []     -> Left $ Error "" "char" "end of line"
                      (x:xs) -> Right (x,xs)

-- | Any integer
readWord :: Read a => InLine a
readWord = do w <- word
              case reads w of
                    [(x,"")] -> return x
                    _        -> fail ("no parse: " ++ show w)

-- | Any integer
integer :: InLine Integer
integer = readWord <?> "integer"

positiveInteger :: InLine Integer
positiveInteger = integer .>. 0  <?> "positive integer"

nonNegativeInteger :: InLine Integer
nonNegativeInteger = integer .>=. 0  <?> "positive integer"

-- | A single space character
space :: InLine Char
space = char .==. ' ' <?> "space"

entireLine :: InLine String
entireLine = I . V $ \s -> Right (s,"")

--------------------------------------------------------------------------------
-- Lines
--------------------------------------------------------------------------------

newtype Lines a = L (Validate (Int,[String]) a) deriving (Functor,Applicative,Monad,MonadValidate)

lineWithOnly :: InLine a -> Lines a
lineWithOnly (I parser) = L . V $ \(line,input) -> case input of
      []     -> errorOn line $ "end of input"
      (x:xs) -> case unV parser x of
                   Left (Error w e f) -> Left $ Error ("Error on line " ++ show line ++ "\n" ++ w) e f
                   Right (a,"") -> Right (a,(line+1,xs))
                   Right (a,xs) -> errorOn line $ "extra input at end of line:\n" ++ show xs
   where errorOn line f = Left $ Error ("Error on line " ++ show line ++ "\n") "" f

lineWith :: LineWithType a b => a -> Lines b
lineWith a = lineWithOnly (lineWithType a)

allLines :: InLine a -> Lines ()
allLines (I parser) = L . V $ go 
  where go (line,[])   = errorOn line $ "end of input"
        go (line,[""]) = Right ((),(line,[""]))
        go (line,x:xs) = case unV parser x of
                   Left (Error w e f) -> Left $ Error ("Error on line " ++ show line ++ "\n" ++ w) e f
                   Right (a,"") -> go (line+1,xs)
                   Right (a,xs) -> errorOn line $ "extra input at end of line:\n" ++ show xs
        errorOn line f = Left $ Error ("Error on line " ++ show line ++ "\n") "" f

class LineWithType a b | a -> b where
    lineWithType :: a -> InLine b
instance LineWithType (InLine a) a where
    lineWithType a = a
instance LineWithType (InLine a,InLine b) (a,b) where
    lineWithType (a,b) = (,) <$> a <* space <*> b
instance LineWithType (InLine a,InLine b,InLine c) (a,b,c) where
    lineWithType (a,b,c) = (,,) <$> a <* space <*> b <* space <*> c

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validate :: Lines a -> String -> Either Error ()
validate (L parser) x = case unV parser (1,lines (x++"\n")) of
     Left e             -> Left e
     Right (_,(_,[""])) -> Right ()
     Right (_,(_,[]))   -> Left $ Error "" "newline at end of input" ""
     Right (_,(l,inp))  -> Left $ Error "" "" ("remaining input on line " ++ show l ++ "\n" ++ take 20 (unlines inp))

validateIO :: Lines a -> IO ()
validateIO a = do
    --hSetBinaryMode stdin True -- no strange stuff
    input <- getContents
    case validate a input of
       Left e -> do hPutStrLn stderr (show e)
                    exitFailure
       Right _ -> return ()

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

times :: Monad m => Integer -> m a -> m [a]
times n x = replicateM  (fromInteger n) x

timesSepBy :: Monad m => Integer -> m b -> m a -> m [a]
timesSepBy n sep x = liftM2 (:) x $ replicateM (fromInteger (n-1)) (sep >> x)

times_ :: Monad m => Integer -> m a -> m ()
times_ n x = replicateM_ (fromInteger n) x

runsOf x = do runs <- lineWith positiveInteger
              runs `times_` x
