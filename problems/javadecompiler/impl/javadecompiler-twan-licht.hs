import Control.Monad

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

type Var = String

data Instruction
   = ILoad   LoadOp
   | IStore  Var
   | IBin    BinOp
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
instance Show LoadOp where
   showsPrec _ (ILoadV v) = showString "iload " . showString v
   showsPrec _ (IConst v) = showString "iconst " . shows v
instance Show BinOp where
   showsPrec _ (IAdd) = showString "iadd"
   showsPrec _ (ISub) = showString "isub"
   showsPrec _ (IMul) = showString "imul"
   showsPrec _ (IDiv) = showString "idiv"

readInstr :: String -> Instruction
readInstr xs = case words xs of
    ["iload",v]  -> ILoad (ILoadV v)
    ["iconst",v] -> ILoad (IConst (read v))
    ["istore",v] -> IStore v
    ["iadd"]     -> IBin IAdd
    ["isub"]     -> IBin ISub
    ["imul"]     -> IBin IMul
    ["idiv"]     -> IBin IDiv
    _            -> error $ "Parse error: " ++ show xs

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

type Expr  = String
type Stack = [Expr]
type State = (Stack,[String])

evalBin :: BinOp -> Expr -> Expr -> Expr
evalBin IAdd x y = "(" ++ x ++ " + " ++ y ++ ")"
evalBin ISub x y = "(" ++ x ++ " - " ++ y ++ ")"
evalBin IMul x y = "(" ++ x ++ " * " ++ y ++ ")"
evalBin IDiv x y = "(" ++ x ++ " / " ++ y ++ ")"

evalInstr :: State -> Instruction -> State
evalInstr (xs,stmts)     (ILoad (ILoadV v)) = (v:xs,stmts)
evalInstr (xs,stmts)     (ILoad (IConst v)) = (show v:xs,stmts)
evalInstr (x:xs,stmts)   (IStore v)         = (xs, (v ++ " = " ++ x ++ ";") : stmts)
evalInstr (x:y:xs,stmts) (IBin o)           = (evalBin o y x:xs,stmts)

eval xs = case foldl evalInstr ([],[]) xs of
            ([],stmts) -> reverse stmts
            xs -> error $ "Stack not empty: " ++ show xs

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

run = do
    n <- readLn
    instrs <- replicateM n (readInstr `fmap` getLine)
    mapM_ putStrLn (eval instrs)

main = flip replicateM_ run =<< readLn
