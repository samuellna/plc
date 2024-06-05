data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

area :: Shape -> Float
area (Circle x) = pi * x * x
area (Rectangle x y) = x * y

-- Tipos de dados recursivos:
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
    deriving (Show, Eq)

-- Funções definidas recursivamente
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

-- Tipos de dados polimórficos:
data Pairs t = Pair t t
pairInt = Pair 4 2-- Int
pairStr = Pair "Samuel" "Junior"

-- Listas:
data List t = Nil | Const t (List t)
    deriving (Show, Eq)
-- Arvores:
data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Show)
-- Defina as seguintes funções

-- showExpr :: Expr -> String
-- toList :: List t -> [t]
-- fromList :: [t] -> List t
-- depth :: Tree t -> Int
-- collapse :: Tree t -> [t]
-- mapTree :: (t -> u) -> Tree t -> Tree u

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = " ( " ++ showExpr e1 ++ " + " ++ showExpr e2 ++ " ) "
showExpr (Sub e1 e2) = " ( " ++ showExpr e1 ++ " - " ++ showExpr e2 ++ " ) "

fromList :: [t] -> List t
fromList xs = foldr (\x -> Const x) Nil xs
-- fromList [] = Nil
-- fromList (x : xs) = Const x (fromList xs)

depth :: Tree t -> Int
depth NilT = 0
depth (Node x left right) = 1 + max (depth left) (depth right)

