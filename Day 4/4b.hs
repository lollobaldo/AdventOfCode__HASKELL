import Debug.Trace

type Minute       = Int
type Count        = Int
type ID           = Int
type MinuteCount  = (Minute , Count)
type Guard        = (ID , MinuteCount)
type Input        = [(Minute , ID , Int)]
type Action       = (ID , Minute , Minute)

emptyMinuteCounts :: [MinuteCount]
emptyMinuteCounts = zip [0..59] $ replicate 60 0

man :: Input -> (ID , Minute)
man inp = maximiser $ assign (guardsList acs []) acs
            where
              acs = structurer inp

maximiser :: [Guard] -> (ID , Minute)
maximiser list = lamb $ helper list (0,0,0)
                  where
                    lamb (a,b,c) = (a,b)
                    helper []                maxx = maxx
                    helper ((id,(m,i)):rest) maxx@(a,b,c)
                              | i > c     = helper rest (id,m,i)
                              | otherwise = helper rest maxx



structurer :: Input -> [Action]
structurer [] = []
structurer ((m,id,_):(n,_,_):rest) = (id,m,n) : structurer rest

guardsList :: [Action] -> [ID] -> [ID]
guardsList [] ret = ret
guardsList ((id,_,_):rest) pre 
                            | id `elem` pre = guardsList rest pre
                            | otherwise = guardsList rest (id:pre)

assign :: [ID] -> [Action] -> [Guard]
assign [] _ = []
assign (id:next) acts = (id , chooser $ minuter id acts emptyMinuteCounts)
                              : assign next acts

chooser :: [MinuteCount] -> MinuteCount
chooser mc = helper mc (0,0)
                where
                  helper [] maxx = maxx
                  helper ((m,i):rest) maxx@(a,b)
                                            | i > b     = helper rest (m,i)
                                            | otherwise = helper rest maxx

minuter :: ID -> [Action] -> [MinuteCount] -> [MinuteCount]
minuter _ [] ret = ret
minuter id ((_,m,n):next) mins = minuter id next $ adder (m,n) mins

adder :: (Minute , Minute) -> [MinuteCount] -> [MinuteCount]
adder (m,n) mins = [if min >= m && min < n
                      then (min,i + 1)
                      else (min,i)
                        | (min,i) <- mins]