module AdventOfCode2017.Day1 where

import qualified Data.Digits as D

readCaptcha :: Integer -> Integer
readCaptcha captcha = foldl f (0,0) captcha
    where f (prev,summ) curr = if prev == curr then
            (curr,summ+curr)
                                else
            (curr,summ)


