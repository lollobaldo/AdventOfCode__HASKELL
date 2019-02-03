type Input = [String]
type Clause = (Char , [Char])

man :: Input -> String
man inp = undefined

parser :: Input -> [Clause]
parser inp = [ | ch <- [a..z]]

adder :: Char -> Char -> [Clause] -> [Clause]
adder ch ad ((c,ls):cs)
                       |  ch == c && ad `elem` ls = 