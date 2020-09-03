-- Abstract syntax tree for calculator language
data Expr a = Lit a 
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    deriving (Show)

-- Eval => eval is a function to evaluation expression (AST) to value 
eval :: Num a => Expr a -> a 
eval expr = case expr of 
    Lit a               -> a 
    Add lhs rhs         -> eval lhs + eval rhs
    Sub lhs rhs         -> eval lhs - eval rhs
    Mul lhs rhs         -> eval lhs * eval rhs

ex1 :: Expr Integer
ex1 = Add (Lit 5) (Lit 3)

ex2 :: Expr Integer
ex2 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)

main :: IO ()
main = do 
    print ex1 
    print $ eval ex1
    print ex2 
    print $ eval ex2 

