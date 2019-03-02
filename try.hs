data Tree a = Leaf a | Node (Tree a) a (Tree a) | UNode a (Tree a) deriving (Show)

reverseFlatten :: [a] -> Tree a
reverseFlatten [x] = (Leaf x)
reverseFlatten [x,y] = UNode y (Leaf x)
reverseFlatten [x,y,z] = Node (Leaf x) y (Leaf z)
reverseFlatten (x:y:xs) = Node (Leaf x) y (reverseFlatten ((xs !! 1) : (head xs) : (drop 2 xs)))

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (UNode l (Leaf x))  = [l,x]
flatten (Node (Leaf l) x r) = l : x : flattenRest r

flattenRest :: Tree a -> [a]
flattenRest (Leaf x)     = [x]
flattenRest (UNode l (Leaf x))  = [l,x]
flattenRest (Node (Leaf l) x r) = x : l : flattenRest r


data Universe a = Universe [a] a [a] deriving (Show, Eq)

f :: Universe Int -> Int
f (Universe _ 0 _) = 0

type State = Int
type DFA = ([State], [Char], State->Char->State, State, [State])

dfaFactory :: DFA
dfaFactory = (states, alphabet, delta, s, fs)
              where
                states = [0,1,2,3,4]
                alphabet= ['1','0','.']
                s = 0
                fs = [3]
                delta 4  _  = 4
                delta 0 '.' = 2
                delta 0  _  = 1
                delta 1 '.' = 3
                delta 1  _  = 1
                delta 2 '.' = 4
                delta 2  _  = 3
                delta 3 '.' = 4
                delta 3  _  = 3

dfaAccept :: DFA -> String -> Bool
dfaAccept (qs,alpha,delta,s,fs) w =
                                    let
                                      q = foldl' delta s w
                                    in
                                      elem q fs