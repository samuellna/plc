sndZero :: [Int] -> Bool -- Funcao que verifica se ha 2 zeros consecutivos
sndZero [] = True
sndZero l
    | head l == 0 = True
    | otherwise = False



sumMachine :: [Int] -> [Int] -- Funcao que retorna a soma entre os numeros da lista ate encontrar dois zeros consecutivos ou a lista acabar
sumMachine [] = []
sumMachine (listNum : listNumS)
    -- | head
    --     where
    --         sumFstZero :: [Int] -> Int
    --         sumFstZero [] = 0
    --         sumFstZero (x : xs)
    --             | x == 0 = 0
    --             | otherwise = x + sumFstZero xs
    | listNum == 0 && not (sndZero listNumS) = listNum : sumMachine listNumS
    | listNum == 0 && sndZero listNumS = [] -- Verificando se ha 2 zeros consecutivos
    | otherwise = [listNum ++ listNumS]

turnIntoList :: Int -> [Int] -- Funcao para transformar a soma feita em sumMachine em uma lista
turnIntoList 0 = []
turnIntoList x = [x]

main :: IO ()
main = do
    lista <- getLine
    print $ sumMachine (read lista :: [Int])