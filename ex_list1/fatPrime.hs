-- Função para verificar se um número é primo
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\x -> n `mod` x /= 0) [2 .. isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- Função para gerar uma lista de números primos
primeNumbers :: [Int]
primeNumbers = filter isPrime [2..]

-- Função para fatorar um número e contar os fatores primos
fatPrime :: Int -> [(Int, Int)]
fatPrime n = fatorizar n primeNumbers
    where
        fatorizar 1 _ = []
        fatorizar m (p:ps)
            | m < p * p = [(m, 1)]
            | m `mod` p == 0 = (p, countFactors m p) : fatorizar (reduce m p) ps
            | otherwise = fatorizar m ps
        countFactors x y = length $ takeWhile (\z -> x `mod` z == 0) (iterate (* y) y)
        reduce x y = until (\z -> z `mod` y /= 0) (`div` y) x

main :: IO ()
main = do
    a <- getLine
    let result = fatPrime (read a :: Int)
    print result