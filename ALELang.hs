module ALELang where

-- Arithmatic and Logic Expression Language

-- AST
data Expr = 
  LitI Integer              -- integer literal
  | LitB Bool
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | If Expr Expr Expr

instance Show Expr where 
  show (LitI n)       = show n 
  show (LitB b)       = show b
  show (And lhs rhs)  = "(" ++ show lhs ++ " && " ++ show rhs ++ ")"
  show (Or lhs rhs)   = "(" ++ show lhs ++ " || " ++ show rhs ++ ")"
  show (Not expr)      = "(" ++ "not " ++ show expr ++ ")"
  show (Add lhs rhs)  = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
  show (Sub lhs rhs)  = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"
  show (Mul lhs rhs)  = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"
  show (If con t f)   = "(" ++ "if " ++ show con ++ " then " ++ show t ++ " else " ++ show f  ++ ")"

-- Evaluation = Expression to value
eval :: Expr -> Either Integer Bool
eval (LitI i)         = Left i
eval (LitB b)         = Right b 
eval (And lhs rhs)    = case (eval lhs, eval rhs) of
                          (Right l, Right r)    -> Right (l && r)
eval (Or lhs rhs)     = case (eval lhs, eval rhs) of
                          (Right l, Right r)    -> Right (l && r)
eval (Not expr)       = case eval expr of
                          Right e               -> Right (not e)
eval (Add lhs rhs)    = case (eval lhs, eval rhs) of 
                          (Left l, Left r)      -> Left (l + r)
eval (Sub lhs rhs)    = case (eval lhs, eval rhs) of 
                          (Left l, Left r)      -> Left (l - r)
eval (Mul lhs rhs)    = case (eval lhs, eval rhs) of 
                          (Left l, Left r)      -> Left (l * r)
eval (If e t f)       = case eval e of 
                          Right rslt            -> eval (if rslt then t else f)


ex1 = LitI 5
ex2 = LitB True 
ex3 = Add (Mul (LitI 2) (LitI 5)) (LitI 3)
ex4 = Not (Or (And (LitB True) (LitB False)) (LitB True))
ex5 = If ex4 ex3 ex1 


main :: IO ()
main = do 
  print ex1 
  print $ eval ex1
  print ex2
  print $ eval ex2 
  print ex3 
  print $ eval ex3
  print ex4 
  print $ eval ex4 
  print ex5 
  print $ eval ex5                           