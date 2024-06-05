mul2 :: [Int] -> [Int] -> [Int]
mul2 x [] = replicate (length x) 0
mul2 [] y = replicate (length y) 0
mul2 (x: xs) (y: ys) = (x * y) : mul2 xs ys

main :: IO ()
main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result