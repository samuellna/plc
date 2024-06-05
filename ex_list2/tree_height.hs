data Tree t = Node t (Tree t) (Tree t) 
              |Nilt
              deriving (Read)

tree_H :: Tree t -> Int
tree_H Nilt = 0 
tree_H (Node a b c) = max (tree_H b) (tree_H c) + 1


main :: IO ()
main = do
       a <- getLine
       let result = tree_H (read a::Tree Int)
       print result