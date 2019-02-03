import Data.Char

remover :: Char -> String -> String
remover _ [] = []
remover ch (s:str)
                    | ch == toLower s = remover ch str
                    | otherwise = s : remover ch str

parser :: String -> String
parser [] = []
parser [a] = [a]
parser (x:y:zs)
                | checker x y = parser zs
                | otherwise   = x : parser (y:zs)
--parser str@(x:xs) = concat [a:[b] | (a,b) <- zip xs str , not $ checker a b]

checker :: Char -> Char -> Bool
checker a b = (toLower a == toLower b) && isUpper a /= isUpper b

whileChanges :: Eq a => (a -> a) -> a -> a
whileChanges f str
                    | str == new = str
                    | otherwise  = whileChanges f new
                        where
                          new = f str

calc :: String -> [(Char , Int)]
calc str = [(ch , length $ whileChanges parser $ remover ch str) | ch <- ['a'..'z']]

man :: String -> Int
man str = minimum' (calc str) (length str)

minimum' :: [(Char,Int)] -> Int -> Int
minimum' [] ret = ret
minimum' ((_,b):rs) d
                          | b < d = minimum' rs b
                          | otherwise = minimum' rs d