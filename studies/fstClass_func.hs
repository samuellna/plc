-- Dada uma função, verificar se ela é crescente de 0 a n

-- isCrescent :: (Int -> Int) -> Int -> Bool
-- isCrescent f n = foldr ((>) . f) True [0..n]
-- isCrescent f n = foldr (>) True (map f [0..n])

-- 1: eleva os itens de uma lista ao quadrado
sqrList :: [Int] -> [Int]
sqrList x = map sqr x
    where
        sqr x = x * x

-- 2: retorna a soma do quadrado dos itens
sumSqrL :: [Int] -> Int
sumSqrL xs = foldr (+) 0 (sqrList xs)

-- 3: manter na lista todos os itens > 0
moreThanZero :: [Int] -> [Int]
moreThanZero xs = filter positive xs
    where
        positive x = x > 0