module Main where

import Data.List
import Data.Char (isUpper, isDigit)
import Debug.Trace
import Data.Time

type Points = Int
type Score  = (Int,Points)

main = do
          start <- getCurrentTime
          contents <- readFile "9.txt"
          print . execA . parse $ contents
          --print . execB . parse $ contents
          stop <- getCurrentTime
          print $ diffUTCTime stop start

--parse by line based on '\n' Char
parse :: String -> (Int, Points)
parse str = (players, rocks)
              where
                tup = map read $ filter isNumber . words $ str
                players = head tup
                rocks   = last tup

isNumber :: String -> Bool
isNumber str = (all (`elem` " +-0123456789")) str && (not . null) str

execA :: (Int, Points) -> Int
execA = winner . play
            where
              play (p, n) = playGame (n+1) 1 p [0] (zip [0..p-1] (repeat 0))

execB :: (Int, Points) -> String
execB = undefined

winner :: [Score] -> Int
winner = maximum . map snd

playGame :: Int -> Int -> Int -> [Int] -> [Score] -> [Score]
playGame 0    _ _ _                      ret = ret
playGame left 1 p [0]                   score = playGame (left-1) 2 p [1,0] score
playGame left 2 p [1,0]                 score = playGame (left-1) 3 p [2,1,0] score
playGame left n p game@(current:one:xs) score
                                                -- | n `mod` 23 == 0 = playGame (left-1) (n+1) p (trace (show newGame) newGame) (addPoints (n `mod` p) (n+addn) score)
                                                -- | otherwise       = playGame (left-1) (n+1) p (trace (show (n:xs ++ [current,one])) (n:xs ++ [current,one])) score
                                                | n `mod` 23 == 0 = playGame (left-1) (n+1) p newGame (addPoints (n `mod` p) (n+addn) score)
                                                | otherwise       = playGame (left-1) (n+1) p (n:xs ++ [current,one]) score
                                                    where
                                                      addn     = (head . snd) split7
                                                      newGame  = newStart ++ newEnd
                                                      newEnd   = fst split7
                                                      newStart = (tail . snd) split7
                                                      split7 :: ([Int], [Int])
                                                      split7 = splitAt (length game - 7) game

playGame' :: Int -> Int -> Int -> [Int] -> [Score] -> [Score]
playGame' 0    _ _ _                      ret = ret
playGame' left 1 p [0]                   score = playGame' (left-1) 2 p [1,0] score
playGame' left 2 p [1,0]                 score = playGame' (left-1) 3 p [2,1,0] score
playGame' left n p game@(current:one:xs) score
                                                -- | n `mod` 23 == 0 = playGame (left-1) (n+1) p (trace (show newGame) newGame) (addPoints (n `mod` p) (n+addn) score)
                                                -- | otherwise       = playGame (left-1) (n+1) p (trace (show (n:xs ++ [current,one])) (n:xs ++ [current,one])) score
                                                | n `mod` 23 == 0 = playGame (left-1) (n+1) p newGame (addPoints (n `mod` p) (n+addn) score)
                                                | otherwise       = playGame (left-1) (n+1) p (n:xs ++ [current,one]) score
                                                    where
                                                      addn     = (head . snd) split7
                                                      newGame  = newStart ++ newEnd
                                                      newEnd   = fst split7
                                                      newStart = (tail . snd) split7
                                                      split7 :: ([Int], [Int])
                                                      split7 = splitAt (length game - 7) game

addPoints :: Int -> Points -> [Score] -> [Score]
addPoints player points scores = addPoints' player points [] scores --trace ("Add " ++ show points ++ " points to player " ++ show player) addPoints' player points [] scores

addPoints' :: Int -> Points -> [Score] -> [Score] -> [Score]
addPoints' player points acc ((p,s):scores)
                                          | player == p = acc ++ ((p,s+points):scores)
                                          | otherwise   = addPoints' player points (acc ++ [(p,s)]) scores