module AdventOfCode2017.Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

maxDiff :: (Ord a, Num a) =>  [a] -> a
maxDiff xs = maximum xs - minimum xs

checksum :: (Ord a, Num a) => [[a]] -> a
checksum = sum . map maxDiff