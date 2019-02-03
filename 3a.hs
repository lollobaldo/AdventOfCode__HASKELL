type Fabric = [[Char]]
type Piece = (Int, Int, Int, Int)
type Pieces = [Piece]

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
count ps = length [x | x <- (concat (final fabric ps)) , x == 'X']
