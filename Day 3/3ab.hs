module Main where
import Data.List

type Fabric = [[Char]]
type Piece = (Int, Int, Int, Int)
type Pieces = [Piece]

data SquareInch = '.' | '1' | 'X'

main = do
          contents <- readFile "3ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
parse :: String -> [String]
parse str = lines str

execA :: [String] -> String
execA = show . checkSum . sumTups . map countRepeats

execB :: [String] -> String
execB = lookForCommons

fabric :: Fabric
fabric = replicate 1001 $ replicate 1001 '.'

insert :: Fabric -> Piece -> Fabric
insert fab (x,y,w,h) = [if not (i > y && i <= (y+h))
                          then row
                          else
                            [if not (j > x && j <= (x+w))
                              then col
                              else
                                if col == '1' then 'X'
                                  else if col == '.' then '1' else col
                            | (j,col) <- zip [1..] row]
                        | (i,row) <- zip [1..] fab]

final :: Fabric -> Pieces -> Fabric
final fab [] = fab
final fab (p:ps) = final (insert fab p) ps

count :: Pieces -> Int
count ps = length [x | x <- (concat (final fabric ps)) , x == 'X']s