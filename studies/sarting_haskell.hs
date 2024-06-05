answer :: Int
answer = 62

-- The variable > answer < is initialized as 62
-- If we do:
    -- answer = 63 (or any value), ghci understand as "i want to create a variable with the name in use".
    -- Therefore, the value of a variable can't be changed. 

greater :: Bool
greater = (answer > 30) -- True

myHusband :: String
myHusband = "Junior"

-- A function that compares three numbers.
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

-- A function that returns the sum of n and all numbers less than n.
reachNumber :: Int -> Int
reachNumber n | n <= 0 = 0
              | otherwise = n + reachNumber (n - 1)

-- A function that calculate the fatorial of a number n.
fatorial :: Int -> Int
fatorial n | n == 0 = 1
           | otherwise = n * fatorial (n - 1)

square :: Int -> Int
square n = n * n
-- If i want to call > square (10) + 2 < or > square 10 + 2 < the function will return 102 ((10 ^ 2) + 2)
