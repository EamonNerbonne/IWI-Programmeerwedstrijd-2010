
import InputGeneration
import Data.Char
import System.Environment

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

type Var = String

data Instruction
   = ILoad   LoadOp
   | IStore  Var
   | IBin    BinOp
   | INeg
data LoadOp
   = ILoadV Var
   | IConst Int
data BinOp
   = IAdd
   | ISub
   | IMul
   | IDiv

instance Show Instruction where
   showsPrec _ (ILoad  x) = shows x
   showsPrec _ (IStore v) = showString "istore " . showString v
   showsPrec _ (IBin   x) = shows x
   showsPrec _ (INeg)     = showString "ineg"
instance Show LoadOp where
   showsPrec _ (ILoadV v) = showString "iload " . showString v
   showsPrec _ (IConst v) = showString "iconst " . shows v
instance Show BinOp where
   showsPrec _ (IAdd) = showString "iadd"
   showsPrec _ (ISub) = showString "isub"
   showsPrec _ (IMul) = showString "imul"
   showsPrec _ (IDiv) = showString "idiv"

{-
data Z
data S n

data Stmt a b where
   IConst :: Integer -> Stmt a (S a)
   IStore :: Var -> Stmt (S a) a
   IAdd   :: Stmt (S (S a)) (S a)


eval :: Num a => 
-}

type Stack = Int

minStack :: Instruction -> Int
minStack (ILoad  _) = 0
minStack (IStore _) = 1
minStack (INeg)     = 1
minStack (IBin _)   = 2

dStack :: Instruction -> Int
dStack (ILoad  _) = 1
dStack (IStore _) = -1
dStack (INeg)     = 0
dStack (IBin _)   = -1

--------------------------------------------------------------------------------
-- Runs
--------------------------------------------------------------------------------

newtype Run = Run [Instruction]

instance Show Run where
    showsPrec _ (Run xs) = shows (length xs)
                         . showString "\n"
                         . showListWithSep "\n" shows xs

--------------------------------------------------------------------------------
-- Test cases : exhaustive
--------------------------------------------------------------------------------

-- possible first instruction of n, at stack size s
-- use int as constant, var as variable
instructionsFor :: Bool -> Int -> Int -> Int -> Var -> [(Int,Instruction)]
instructionsFor includeNeg n s int var
    = [ (1,  ILoad (IConst int) ) | n-1 >= s+1 ]
   ++ [ (1,  ILoad (ILoadV var) ) | n-1 >= s+1 ]
   ++ [ (-1, IStore var) | s == 1 && n-1 /= 1 ] -- TODO s == 1 or s >= 1?
   ++ [ (0,  INeg)   | s >= 1 && n-1 >= s && includeNeg ]
   ++ [ (-1, IBin i) | s >= 2 && n-1 >= s-1, i <- [IAdd,ISub,IMul,IDiv] ]

mkExhaustive :: Bool -> Int -> Int -> [] [Instruction]
mkExhaustive includeNeg 0 0  = return []
mkExhaustive includeNeg 0 _s = mzero
mkExhaustive includeNeg n s 
  | n < s = mzero -- can't possibly recover
  | otherwise = do
         (ds,i) <- instructionsFor includeNeg n s  n [chr (96 + n)]
         after <- mkExhaustive includeNeg (n-1) (s+ds)
         return $ i : after

exhaustiveRuns includeNeg =
  [ Run i
  | n <- [1..7]
  , i <- mkExhaustive includeNeg n 0
  ]

--------------------------------------------------------------------------------
-- Test cases : random
--------------------------------------------------------------------------------

mkRandom' :: Bool -> Int -> Int -> Rand [Instruction]
mkRandom' includeNeg 0 0  = return []
mkRandom' includeNeg 0 _s = error "stack not empty"
mkRandom' includeNeg n s 
  | n < s = error "stack too large" -- can't possibly recover
  | otherwise = do
         rnameLen <- getRandomR (1,5)
         rname <- replicateM rnameLen $ getRandomR ('a','z')
         rconst <- getRandomR (0,1000000)
         let choices = instructionsFor includeNeg n s  rconst rname
         (ds,i) <- randomChoice choices
         after <- mkRandom' includeNeg (n-1) (s+ds)
         return $ i : after

mkRandom :: Bool -> Int -> Rand Run
mkRandom includeNeg n = Run <$> mkRandom' includeNeg n 0

randomRuns includeNeg =
      [ runRandom seed $ mkRandom includeNeg 100    | seed <- take 10 [0x90210..] ]
   ++ [ runRandom seed $ mkRandom includeNeg 1000   | seed <- take 10 [0xf00ba7..] ]
   ++ [ runRandom seed $ mkRandom includeNeg 5432   | seed <- take 3 [0x4513453..] ]
   ++ [ runRandom seed $ mkRandom includeNeg 10000  | seed <- take 3 [0xBABEB00B..] ]
   ++ [ runRandom seed $ mkRandom includeNeg 99998  | seed <- take 1 [0xB00B1E5..] ] -- note: must be even if ineg is not allowed

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = do
     args <- getArgs
     case args of
       [] -> printRuns (runs True)
       ["licht"] -> printRuns (runs False)
       _ -> error "Usage: MkInput [licht]"

runs includeNeg = exhaustiveRuns includeNeg ++ randomRuns includeNeg
