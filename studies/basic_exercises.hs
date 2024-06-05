-- Exercise 1: create a function that using a weight and a height, returns the imc.  
sqr :: Float -> Float
sqr n = n * n

imc :: Float -> Float -> Float
imc w h | (w == 0) || (h == 0) = -1
        | otherwise = w / sqr h

-- Exercise 2: 
lessNum :: Float -> Float -> Float -> Bool
lessNum a b c | (a < b) && (b < c) = True
              | otherwise = False

-- Escreva uma função para retornar, em forma de tabela, todas as vendas da semana 0 até a semana n, incluindo o total e a média de vendas no período. Usem as
-- funções definidas previamente e defina novas funções que achar necessário.

-- Função que retorna a tabela de vendas.
sales :: Int -> Int
sales n | n == 0 = 0
        | otherwise = 1 + (n * 2)

-- Função que calcula o total de vendas da semana 0 até a semana n
totalSales :: Int -> Int
totalSales n | n < 0 = 0
             | otherwise = sales n + totalSales (n - 1)

-- Função que armazena em uma lista as vendas da semana 0 até a n
salesTable :: Int -> [Int]
salesTable table = [sales 0..sales table]

-- Defina a função addEspacos que produz um string com uma quantidade n de espaços.
addSpaces :: Int -> String
addSpaces n = [' ' | _ <- [1..n]]

-- Função que dá um shift-right em uma string.
toRight :: Int -> String -> String
toRight n str = addSpaces n ++ str

-- Função que recebe um numero n e retorna uma lista de 0 até n-1 em que cada número é convertido em string. 
listWeeksStr :: Int -> [String]
listWeeksStr n = map show [0..n - 1]

-- Função que recebe uma lista de inteiros e retorna uma lista de strings.
listSalesTableStr :: [Int] -> [String]
listSalesTableStr tableStr 
        | null tableStr = []
        | otherwise = show (head tableStr) : listSalesTableStr (tail tableStr)

printWeeksandSales :: [String] -> String
printWeeksandSales [] = []
printWeeksandSales str = printWeek (head str) ++ printWeeksandSales (tail str)
        where
                printWeek :: String -> String
                printWeek week = "\n" ++ toRight 2 week ++ addSpaces (8 - length week)

header = "Semana    Vendas"

printTotal :: Int -> IO()
printTotal n = putStr (header ++
                        printWeeksandSales (listWeeksStr n) ++
                        printWeeksandSales (listSalesTableStr (salesTable n)))