putSpaces :: String -> String -- Funcao que troca o ';' da string por ' '
putSpaces [] = []
putSpaces (x : xs)
    | x == ';' = " " ++ putSpaces xs -- Trocando o ';' por ' ' para poder usar a funcao > words < mais abaixo.
    | otherwise = x : putSpaces xs

pickPrices :: [String] -> Int -> [Double] -- Funcao que retorna apenas os precos da fatura
pickPrices [] _ = []
pickPrices (x : xs) 0 = []
pickPrices (x : xs) i
    | i `mod` 4 == 0 = read x : pickPrices xs (i + 1) -- Os valores estao sempre num intervalo de 4 strings
    | otherwise = pickPrices xs (i + 1)

minMaxCartao :: String -> (Double, Double) -- Funcao que retorna o menor e o maior preco da fatura
minMaxCartao [] = (0, 0)
minMaxCartao str = 
    let l = pickPrices (words (putSpaces str)) 1 in (minimum l, maximum l)

main :: IO ()
main = do
    a <- getLine
    let result = minMaxCartao a
    print result