addSpaces :: Int -> String
addSpaces n = [' ' | _ <- [1..n]]

-- Função que dá um shift-right em uma string.
toRight :: Int -> String -> String
toRight n str = addSpaces n ++ str

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry toRight . parseInput