-- Defina uma função que, dado um número inteiro, determina se ele é primo ou não.

-- Fazendo com List comprehension
divisores :: Int -> [Int]
divisores x = [numeros | numeros <- [1..x], x `mod` numeros == 0]

-- Fazendo de forma recursiva
qtdDivisores :: Int -> Int -> Int
qtdDivisores a counter
        | a <= 0 = 1000
        | a == 1 = 1
        | otherwise = calcularDiv a (a - 1) counter
        where
            calcularDiv :: Int -> Int -> Int -> Int
            calcularDiv a divisor cont
                | divisor == 1 = cont + 1
                | a `mod` divisor == 0 = calcularDiv a (divisor - 1) (cont + 1)
                | otherwise = calcularDiv a (divisor - 1) cont
                        
ehPrimo :: Int -> Bool
ehPrimo n | qtdDivisores n 0 == 2 = True
          | otherwise = False

-- Defina uma calculadora de fatorial
fat :: Int -> Int
fat x | x <= 0 = 1
      | x == 1 = 1
      | x > 1 = x * fat (x - 1)

-- Defina uma função que determina se dois numeros sao primos entre si
primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y
    | x == y = False
    | (x == y + 1) || (y == x + 1) = True
    | otherwise = compareDivs [numeros | numeros <- divisores x, numeros `elem` divisores y]
    where
        compareDivs :: [Int] -> Bool
        compareDivs list_divs | length list_divs == 1 = True
                              | otherwise = False

        
-- Defina uma função que determina se 4 numeros são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = allEqual a b c && allEqual b c d
    where
        allEqual :: Int -> Int -> Int -> Bool
        allEqual x y z = (x == y) && (y == z)