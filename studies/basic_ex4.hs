-- Return the list with
double :: [Int] -> [Int]
double [] = []
double (x : xs) = 2 * x : double xs

-- Return if a number is in a list of integers
member :: [Int] -> Int -> Bool
member [] _ = False
member l x = x `elem` l

-- Returns all digits in the string
digits :: String -> String 
digits [] = []
digits (x : xs) 
    | digit x = x : digits xs
    | otherwise = digits xs
    where 
        digit :: Char -> Bool
        digit c = (c == '0') || (c == '1') || (c == '2') || (c == '3') || (c == '4') || (c == '5') || (c == '6') || (c == '7') || (c == '8') || (c == '9')

-- Sum the elements of two lists
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs x [] = x
sumPairs [] y = y
sumPairs (x : xs) (y : ys) = (x + y) : sumPairs xs ys