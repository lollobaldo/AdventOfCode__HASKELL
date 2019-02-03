type NMet = Int
type NNod = Int
type Metadata = Int
data Tree = Node NNod NMet [Tree] [Metadata] deriving (Eq, Ord, Show)

type Input = [Int]

tryInp :: Input
tryInp = [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]

treeParser :: Input -> Tree
treeParser (n:m:ts) = Node n m (helper inp) (drop b ts)
                        where
                          b = (length ts - m)

helper :: Input -> [Tree]
helper [] _ = []
helper (0:m:ts) = [Node 0 m [] (drop b ts)]
helper (n:m:ts) = [Node n m [makeTrees t | t <- zip [1..n]] (drop b ts)]
  where
    b = (length ts - m)

makeTrees :: Input -> Int -> [Tree]
makeTrees (n:m:ts) n = Node n m makeTrees 

man :: Input -> Int
man (a:b:xs) = 