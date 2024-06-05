import Data.List (nub)

allEqual :: Eq t => t -> t-> t-> Bool
allEqual n m p = (n == m) && (m == p) 

-- Crie uma função agrupar que recebe uma lista de listas de valores
-- de um tipo t que podem ser comparados para saber se são iguais e
-- devolve uma lista de pares (t, Int) onde o primeiro elemento é um
-- valor do tipo t que existe em pelo menos uma das sub-listas da
-- entrada e o segundo é o número de ocorrências desse valor nas sub
-- listas:

removeOcorrencias :: Eq t => [t] -> t -> [t]
removeOcorrencias [] _ = []
removeOcorrencias (x : xs) y
    | x == y = removeOcorrencias xs y
    | otherwise = x : removeOcorrencias xs y
    
ocorrencias :: Eq t => [t] -> t -> Int
ocorrencias [] _ = 0
ocorrencias (x : xs) y
    | x == y = 1 + ocorrencias xs y
    | otherwise = ocorrencias xs y

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar xss = [(x, ocorrencias (concat xss) x) | x <- nub (concat xss)]

-- agrupar :: Eq t => [t] -> [(t, Int)]
-- agrupar [] = []
-- agrupar (x : xs) = (x, ocorrencias (x : xs) x) : agrupar (removeOcorrencias (x : xs) x)