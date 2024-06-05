htob :: String -> String
htob [] = []
htob (x : xs) = bin x hexaB ++ htob xs
    where
        hexaB = [ -- Tabela com as equivalencias entre os numeros hexa e bin.
            ('0', "0000"), 
            ('1', "0001"), 
            ('2', "0010"), 
            ('3', "0011"),
            ('4', "0100"), 
            ('5', "0101"), 
            ('6', "0110"), 
            ('7', "0111"),
            ('8', "1000"), 
            ('9', "1001"), 
            ('A', "1010"), 
            ('B', "1011"),
            ('C', "1100"), 
            ('D', "1101"), 
            ('E', "1110"), 
            ('F', "1111")
            ]

        bin :: Char -> [(Char, String)] -> String
        bin _ [] = []
        bin c listTuple
            | c == fst (head listTuple) = snd (head listTuple)
            | otherwise = bin c (tail listTuple)

main :: IO ()
main = do
    s <- getLine
    let result = htob s
    print result