import Debug.Trace
type Fabric = [[String]]
type Piece = (Int,Int, Int, Int, Int)
type Pieces = [Piece]
type Complete = (Int,Int,Int,Int,Int,Int)

man :: Pieces -> Int
man ps = final [] $ formatter ps

final :: Pieces -> Pieces -> Int
final _ [] = 0
final pre (p@(id,_,_,_,_):ps)
              | traceShow (p) (check p (pre++ps)) = id
              | otherwise = final (pre++[p]) ps

formatter ps = [format p | p <- ps]
format (id,x,y,w,h) = (id,x+1,y+1,x+w,y+h)

check :: Piece -> Pieces -> Bool
check p@(_,x,y,w,h) ps = and [notIntersect p s | s <-ps]

notIntersect :: Piece -> Piece -> Bool
notIntersect (_,x,y,w,h) (_,a,b,c,d) = (x > c || a > w ||
                                        y > d || b > h)