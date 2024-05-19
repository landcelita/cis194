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

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  mul x y = x * y
  add x y = x + y
  lit x = x

instance Expr Bool where
  mul x y = x && y
  add x y = x || y
  lit x = x > 0

instance Expr MinMax where
  mul (MinMax x) (MinMax y) = MinMax(min x y)
  add (MinMax x) (MinMax y) = MinMax(max x y)
  lit x = MinMax(x)

instance Expr Mod7 where
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  lit x = Mod7(x)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
