module AdventOfCode2017.Day1 where

import qualified Data.Digits as D

readCaptcha ::Integral a =>  ([a] -> [(a,a)]) -> a -> a
readCaptcha f = sum . map fst . filter (\ x -> fst x == snd x) . f . D.digits 10
    
getNext :: [a] -> [(a,a)]
getNext xs = zip xs (tail xs) ++ [(last xs, head xs)]

getHalfwayItem :: [a] -> [(a,a)]
getHalfwayItem xs = map f [0 .. l - 1]
    where l = length xs          
          f p = (xs !! p, xs !! mod (p + div l 2) l)


solutionDay1 :: IO Integer
solutionDay1 = undefined