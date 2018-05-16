{-# LANGUAGE BangPatterns #-}

module AdventOfCode2017.Day5 where

import qualified Data.List.Zipper as LZ
import           Data.List.Zipper (Zipper)


readInput :: FilePath -> IO (Zipper Int)
readInput filepath = do
    input <- readFile filepath
    let l = lines input
    let n = map read l :: [Int]
    return . LZ.fromList $ n


step :: (Int -> Int) -> Zipper Int -> Maybe (Zipper Int)
step updater zipper = do
    steps <- LZ.safeCursor zipper
    let zipper' = LZ.replace (updater steps) zipper
    let lr = if steps >= 0
             then replicate steps LZ.right
             else replicate (-steps) LZ.left
    return $ foldl (flip ($)) zipper' lr

baseUpdater :: Int -> Int
baseUpdater = (+1)

bonusUpdater :: Int -> Int
bonusUpdater n = if n > 2
    then n - 1
    else n + 1

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = case f a of
    Nothing -> []
    (Just b) -> b : iterateMaybe f b