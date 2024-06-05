data Expr = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    deriving Show

exemplo :: Expr
exemplo = Add (Lit 3) (Sub (Lit 4) (Lit 2))
