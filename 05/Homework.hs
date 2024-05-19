import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x =
    case parseExp Lit Add Mul x of
      Just expr -> Just (eval expr)
      Nothing -> Nothing

class Expr a where
  mul :: a -> a -> a
  add :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  mul x y = Mul x y
  add x y = Add x y
  lit x = Lit x

reify :: ExprT -> ExprT
reify = id
