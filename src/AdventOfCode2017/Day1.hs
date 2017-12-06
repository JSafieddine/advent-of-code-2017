module AdventOfCode2017.Day1 where

import qualified Data.Digits as D

readCaptcha :: Integer -> Integer
readCaptcha input = let captcha = D.digits 10 input in
    snd . foldl f (last captcha,0) $ captcha
    where
        f (prev,summ) curr =
            if prev == curr then
                (curr,summ+curr)
            else
                (curr,summ)


getHalfwayItem :: [a] -> Int -> (a,a)
getHalfwayItem list pos = undefined

solutionDay1 :: IO Integer
solutionDay1 = undefined