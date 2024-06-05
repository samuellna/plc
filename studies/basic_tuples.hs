-- Tuple is a data structure that represents two or more values
-- For example, in a map, a position is represented with two values, x and y.

intP :: (Int, Int)
intP = (30, 40)

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

type Name = String
type Age = Int
type Phone = String
type Person = (Name, Age, Phone)

name :: Person -> Name
name (n, a, p) = n

-- Bhaskara = -b +- sqrt(b^2 - 4 * a * c) / 2 * a

numR :: Float -> Float -> Int
numR b2 ac4
    | b2 > ac4 = 2
    | b2 == ac4 = 1
    | otherwise = 0

x1 :: Float -> Float -> Float -> Float
x1 a b c = (-1) * b + ()

-- roots :: Float -> Float -> Float -> (Int, Float, Float)