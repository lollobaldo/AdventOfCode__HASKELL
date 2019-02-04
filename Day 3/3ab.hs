module Main where
import Data.List.Split

type Fabric = [[SquareInch]]
type Piece = (Int, Int, Int, Int)
type Pieces = [Piece]

data SquareInch = Free | Taken | Conflict deriving (Eq,Show)

main = do
          contents <- readFile "3ab.txt"
          --print $ parse contents
          putStrLn $ execA $ parse contents
          putStrLn $ execB $ parse contents

--parse by line based on '\n' Char
parse :: String -> [Piece]
parse str = [(read x,read y,read a,read b) | [_,_,_,x,y,_,a,b] <- map (splitOneOf "@:,x ") $ lines str]

-- parserHelper :: [String] -> Piece
-- parserHelper [_,_,_,x,y,_,a,b] = ()

execA :: [Piece] -> String
execA = show . countConflicts

execB :: [Piece] -> String
execB ps = "head"

fabric :: Fabric
fabric = replicate 1001 $ replicate 1001 Free

insert :: Fabric -> Piece -> Fabric
insert fab (x,y,w,h) = [if not (i > y && i <= (y+h))
                          then row
                          else
                            [if not (j > x && j <= (x+w))
                              then col
                              else
                                if col == Taken then Conflict
                                  else if col == Free then Taken else col
                            | (j,col) <- zip [1..] row]
                        | (i,row) <- zip [1..] fab]

final :: Fabric -> Pieces -> Fabric
final fab [] = fab
final fab (p:ps) = final (insert fab p) ps

countConflicts :: Pieces -> Int
countConflicts ps = length [x | x <- (concat (final fabric ps)) , x == Conflict]