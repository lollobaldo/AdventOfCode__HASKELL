import Data.Char

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

man :: String -> Int
man str = length $ whileChanges parser str