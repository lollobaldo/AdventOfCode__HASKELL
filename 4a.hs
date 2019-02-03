import Debug.Trace

list = zip [0..3491] $ replicate 3941 0

parser1 :: [(Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
parser1 [] ls = ls
parser1 ((m,id,_):(n,_,_):xs) ls = parser1 xs $ adder id (n-m) ls

adder :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
adder id n ls = addone id n [] ls
                  where
                    addone id n pre [] = error ("id " ++ show id ++ " not found")
                    addone id n pre (tup@(a,b):rs)
                        | id == a   = pre ++ [(a,b+n)] ++ rs
                        | otherwise = addone id n (pre++[tup]) rs

man :: [(Int,Int,Int)] -> (Int,Int)
man ls = maximum' (parser1 ls list) (0,0)

maximum' :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
maximum' [] ret = ret
maximum' ((a,b):rs) maxx@(c,d)
                          | b > d = maximum' rs (a,b)
                          | otherwise = maximum' rs maxx

lister :: [(Int,Int,Int)] -> Int -> [(Int,Int)]
lister [] _ = []
lister ((m,id,_):(n,_,_):rs) idd
                            | id == idd = (m,n) : lister rs idd
                            | otherwise = lister rs idd

mList = zip [0..59] $ replicate 60 0

adder':: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
adder' _ [] = []
adder' (a,b) ((c,d):rs)
                        | a <= c && c < b = (c,d+1) : adder' (a,b) rs
                        | otherwise = (c,d) : adder' (a,b) rs


--ID = 1987
mann :: [(Int,Int)] -> Int
mann ls = fst $ maximum' (fain ls mList) (0,0)

fain :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
fain [] ls = ls
fain ((m,n):xs) ls = fain xs $ adder' (m,n) ls