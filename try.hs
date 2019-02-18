data Tree a = Leaf a | Node (Tree a) a (Tree a) | UNode a (Tree a) deriving (Show)

reverseflatten :: [a] -> Tree a
reverseflatten [x] = (Leaf x)
reverseflatten [x,y] = UNode y (Leaf x)
reverseflatten [x,y,z] = Node (Leaf x) y (Leaf z)
reverseflatten (x:y:xs) = Node (Leaf x) y (reverseflatten ((xs !! 1) : (head xs) : (drop 2 xs)))

divv :: Int -> Int -> Float
divv x y = x/y