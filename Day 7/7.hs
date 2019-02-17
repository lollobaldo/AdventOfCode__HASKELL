module Main where

import Data.List.Split
import Data.List
import Data.Char (isUpper)

type Clause = (Instruction , Instruction)
type Requirement = (Instruction , [Instruction])
type Instruction = Char

main = do
          contents <- readFile "7.txt"
          print . execA . parse $ contents
          --print . execB . parse $ contents

--parse by line based on '\n' Char
parse :: String -> [Clause]
parse str = [(a, b) | [a,b] <- map (tail . filter isUpper) $ lines str]

execA :: [Clause] -> String
execA = satisfy [] . putTogether

execB :: [Clause] -> String
execB = undefined

emptyReqs :: [Requirement]
emptyReqs = zip ['A'..'Z'] (cycle [[]])

putTogether :: [Clause] -> [Requirement]
putTogether = foldl' (\reqs cl -> insertInReqs cl reqs) emptyReqs

insertInReqs :: Clause -> [Requirement] -> [Requirement]
insertInReqs (a,b) (cl@(c,str):reqs)
                                      | c == b    = (c,a:str):reqs
                                      | otherwise = cl : insertInReqs (a,b) reqs

removeFromReqs :: Instruction -> Requirement -> Requirement
removeFromReqs ch req@(c,str)
                              | ch `elem` str = (c,str\\[ch])
                              | otherwise     = req

satisfy :: [Requirement] -> [Requirement] -> String
satisfy _ [] = ""
satisfy acc (req@(c,str):reqs)
                              | str == "" = c : satisfy [] (map (removeFromReqs c) (acc ++ reqs))
                              | otherwise = satisfy (acc ++ [req]) reqs