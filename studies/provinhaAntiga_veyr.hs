countZeros :: [Int] -> Int
countZeros [] = 0
countZeros (x : xs)
    | x /= 0 = 0
    | otherwise = 1 + countZeros xs

rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (x : xs)
    | x == 0 = x : countZeros (x : xs) : rlencode0 (skipZeros xs)
    | otherwise = x : rlencode0 xs
    where 
        skipZeros :: [Int] -> [Int]
        skipZeros [] = []
        skipZeros (x : xs)
            | x == 0 = skipZeros xs
            | otherwise = xs

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 (0 : x : xs) = replicate x 0 ++ rldecode0 xs
rldecode0 (x: xs) = x : rldecode0 xs

reptLetras :: String -> Int -> String
reptLetras [] _ = []
reptLetras (x1 : x2 : xs) n
    | x1 == x2 = reptLetras (x2 : xs) (n + 1)
    | otherwise = show n

rlencodeLetras :: String -> String
rlencodeLetras [] = []
rlencodeLetras [x] = [x]
rlencodeLetras (x1 : x2 : xs)
    | x1 == x2 = (x1 : reptLetras (x1 : x2 : xs) 1) ++ rlencodeLetras (skipL xs)
    where
        skipL :: String -> String
        skipL [] = []
        skipL [x] = []
        skipL (x : xs)
            | x == head xs = skipL xs
            | otherwise = xs

-- rlencodeLetras 
-- rldecodeLetras :: String -> String