module Main where
import Data.List

main = do
          contents <- readFile "2ab.txt"
          print . execA . parse $ contents
          print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> [String]
parse str = lines str

execA :: [String] -> Int
execA = checkSum . sumTups . map countRepeats

execB :: [String] -> String
execB = lookForCommons

checkSum :: (Int,Int) -> Int
checkSum (a,b) = a*b

sumTups :: [(Int,Int)] -> (Int,Int)
sumTups = foldr (\(x,y) (a,b) -> (x+a,y+b)) (0,0)

countRepeats :: String -> (Int,Int)
countRepeats str = helperCount (sort str) (0,0)

helperCount :: String -> (Int,Int) -> (Int,Int)
helperCount (ch1:ch2:ch3:chs) (0,0)
                                    | ch1 == ch2 && ch2 == ch3 = helperCount chs           (0,1)
                                    | ch1 == ch2               = helperCount (ch3:chs)     (1,0)
                                    | otherwise                = helperCount (ch2:ch3:chs) (0,0)
helperCount (ch1:ch2:ch3:chs) (1,0)
                                    | ch1 == ch2 && ch2 == ch3 = (1,1)
                                    | otherwise                = helperCount (ch2:ch3:chs) (1,0)
helperCount (ch1:ch2:chs)     (0,1)
                                    | ch1 == ch2               = (1,1)
                                    | otherwise                = helperCount (ch2:chs)     (0,1)
helperCount [_] ret = ret
helperCount (ch:chs) ret = helperCount chs ret

commonChars :: String -> String -> String
commonChars s1 s2 = if length common == (length s1 - 1)
                      then common
                    else
                      ""
                    where
                      common = [a | (a,b) <- zip s1 s2 , a == b]

lookForCommons :: [String] -> String
lookForCommons (s:str) = if compareAll == "" then lookForCommons str else compareAll
                            where
                              compareAll = concat [commonChars s x | x <- str]