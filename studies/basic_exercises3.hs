boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs] 

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

-- This function receives a List that contains a list of integers and removes all odd numbers without
removeOddNumListinList :: [[Int]] -> [[Int]]
removeOddNumListinList listNum2 = [[x | x <- listNum, even x] | listNum <- listNum2]