module AdventOfCode2017.Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


readMatrix :: Read a => T.Text -> [[a]]
readMatrix = map (map (read . T.unpack ) . T.split (== '\t')) . T.lines

maxDiff :: (Ord a, Num a) =>  [a] -> a
maxDiff xs = maximum xs - minimum xs

divable :: Integral a => [a] ->  a
divable xs = head [div x y | x <- xs , y <- xs, x /= y, mod x y == 0]


checksum :: (Ord a, Num a) => ([a] -> a) -> [[a]] -> a
checksum f = sum . map f