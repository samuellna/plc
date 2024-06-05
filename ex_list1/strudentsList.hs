bSort :: [String] -> [String]
bSort [] = []
bSort str = sort str (length str - 1) -- Chamando a funcao que realiza trocas
    where
        sort :: [String] -> Int -> [String]
        sort strL 0 = strL
        sort [] _ = []
        sort strL n = sort (b strL) (n - 1) -- Chamando a funcao que empurra o ultimo elemento para o final (n - 1) vezes

        b :: [String] -> [String]
        b [x] = [x]
        b (x1:x2:xs)
            | x1 > x2 = x2 : b (x1 : xs)
            | otherwise = x1 : b (x2 : xs)

main :: IO ()
main = do
    a <- getLine
    let result = bSort (read a :: [String])
    print result