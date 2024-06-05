data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

inOrder :: Tree t -> [t]
inOrder Nilt = []
inOrder (Node value left_node right_node) = inOrder left_node ++ [value] ++ inOrder right_node

turnIntoMod5:: [Int] -> [Int]
turnIntoMod5 xs = map (`mod` 5) xs

convertString :: [Int] -> String
convertString [] = []
convertString (x : xs)
    | x == 0 = 'E' : convertString xs
    | x == 1 = 'M' : convertString xs
    | x == 2 = 'A' : convertString xs
    | x == 3 = 'C' : convertString xs
    | otherwise = 'S' : convertString xs

splitArray :: String -> [String]
splitArray [] = []
splitArray xs = take 8 xs : splitArray (drop 8 xs)

dna1 :: Tree Int -> [String]
dna1 t = splitArray (convertString(turnIntoMod5(inOrder t)))

main :: IO ()
main = do
  input <- getLine
  let result = dna1 (read input :: Tree Int)
  print result