listAfterZero :: [Int] -> [Int]
listAfterZero [] = []
listAfterZero (x : xs) 
    | x == 0 && null xs = []
    | x == 0 && head xs == 0 = []
    | x == 0 = xs
    | otherwise = listAfterZero xs

sumUntilZero :: [Int] -> Int
sumUntilZero [] = 0
sumUntilZero (x : xs)
    | x == 0 = 0
    | otherwise = x + sumUntilZero xs

sumMachine :: [Int] -> [Int]
sumMachine [] = []
sumMachine listNum
    | head listNum == 0 && head (tail listNum) == 0 = []
    | otherwise = sumUntilZero listNum : sumMachine (listAfterZero listNum)

sumMachine' :: [Int] -> [Int]
sumMachine' [] = []
sumMachine' x
    | head x == 0 && null (tail x) = []
    | head x == 0 && head (tail x) == 0 = []
    | head x == 0 = sumMachine (tail x)
    | otherwise = sumMachine x

main :: IO ()
main = do
    lista <- getLine
    print $ sumMachine' (read lista :: [Int])