-- 1) A sequência de Fibonacci é uma sequência de números que começa com os
-- números zero e um e os números seguintes são dados pela soma dos dois números
-- anteriores. Ela é infinita. Seus primeiros números são 0,1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
-- 144, 233, 377, 610, 987, 1597, 2584, ...
-- Escreva o código necessário para gerar uma lista infinita contendo os números de Fibonacci,
-- a partir da lista de entrada contendo zero e um.


-- 2. (2,5 pontos) Escreva uma função que recebe duas listas já ordenadas e faz o merge
-- (combina) as duas listas, dando como resultado uma lista também ordenada. O algoritmo
-- deve levar em conta que as duas listas de entrada já estão ordenadas.

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | x > y = y : merge (x : xs) ys
    | otherwise = x : y : merge xs ys