import Data.String

putSpaces :: String -> String
putSpaces [] = []
putSpaces (x : xs) 
    | x == ';' = " "  ++ putSpaces xs
    | otherwise =  x : putSpaces xs

turnIntoList :: String -> [String]
turnIntoList [] = []
turnIntoList a = words(putSpaces a)

month ::[String] -> Int ->[String]
month [] i = []
month n 0 = []
month (x : xs) i 
    | i `mod` 4 /= 0 && even i = x : month xs(i + 1)
    | otherwise = month xs (i + 1)
                   
prices ::[String] -> Int ->[Double]
prices [] i = []
prices n 0 = []
prices (x : xs) i 
    | i `mod` 4 == 0 =  read x : prices xs (i + 1)
    | otherwise = prices xs (i + 1)

createTuple :: [String] -> [Double] -> [(String , Double)]
createTuple [] b = []
createTuple a [] = []
createTuple (x : xs) (y : ys) = (x, y) : createTuple xs ys 

juntaTudo:: String -> [(String , Double)]
juntaTudo a =  createTuple(month (turnIntoList a) 1 )  (prices(turnIntoList a ) 1)

sumMonth :: [(String , Double)]-> String -> [Double]
sumMonth [] a = []
sumMonth a [] = []
sumMonth (x : xs) y 
    | fst x == y = snd x : sumMonth xs y
    | otherwise = sumMonth xs y

logMes :: String -> String -> Double
logMes a b = foldl (+) 0  (sumMonth (juntaTudo b) a)

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result
