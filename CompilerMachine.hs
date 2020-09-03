module CompilerMachine where 

-- Abstract syntax tree for calculator language
data Expr a = Lit a 
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving (Show)

-- The instruction of super simple stack machine 
data InstructionSetMachine a = Push a -- place literal on top of 
    | DoAdd
    | DoSub
    | DoMul 
    deriving (Show)

-- Program => a program is a sequence of instructions
type Program n = [InstructionSetMachine n]

-- Stack => a stack is a sequence of values
type Stack a = [a]

-- Eval => eval is a function to evaluation expression (AST) to value 
eval :: Num a => Expr a -> a 
eval expr = case expr of 
    Lit a               -> a 
    Add lhs rhs         -> eval lhs + eval rhs
    Sub lhs rhs         -> eval lhs - eval rhs
    Mul lhs rhs         -> eval lhs * eval rhs
    
-- Compile => translate expression of AST into program (machine code)
compile :: Expr a -> Program a
compile (Lit n)             = [Push n] 
compile (Add lhs rhs)       = compile lhs ++ compile rhs ++ [DoAdd] 
compile (Sub lhs rhs)       = compile lhs ++ compile rhs ++ [DoSub]
compile (Mul lhs rhs)       = compile lhs ++ compile rhs ++ [DoMul] 

-- Run a program against an initial stack 
-- Run program means Operating system reads all instruction set and run it on machine 
-- (leaves the program result on the stack top)
run :: Num a => Stack a -> Program a -> Stack a
run st          []                  = st 
run st          ((Push n):res)      = run (n:st)        res 
run (x:y:st)    (DoAdd:res)         = run (y + x:st)    res
run (x:y:st)    (DoSub:res)         = run (y - x:st)    res
run (x:y:st)    (DoMul:res)         = run (y * x:st)    res
run _           _                   = error "illegal instruction"

ex1 :: Expr Integer
ex1 = Add (Lit 5) (Lit 3)

ex2 :: Expr Integer
ex2 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)


main :: IO ()
main = do 
    print ex1 
    print $ eval ex1
    print $ compile ex1 
    print $ (run [] . compile) ex1 
    print ex2 
    print $ eval ex2 
    print $ compile ex2
    print $ (run [] . compile) ex2 