import System.IO ()

main :: IO ()
main = do
    putStrLn "Input filepath: "
    in_filepath <- getLine
    
    putStrLn "Output filepath: "
    out_file <- getLine
    
    in_file <- readFile in_filepath
    writeFile out_file in_file
    
    content_out <- readFile out_file
    putStrLn content_out    

    putStrLn "Do you want to make another copy? (Y / N) "
    answer <- getChar
    if answer == 'S' then main else return ()
