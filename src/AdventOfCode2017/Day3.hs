module AdventOfCode2017.Day3 where

-- 1 ^= (0,0)
-- 2 ^= (1,0)
-- 3 ^= (1,1)
-- 4 ^= (0,1)
-- 5 ^= (-1,1)
-- 6 ^= (-1,0)

grid :: Int -> (Integer,Integer)
grid n = foldl add (0,0) $ take (n+1) path
    where dirs = concat $ repeat $ map (\ x -> (round . cos $ pi / 2 * x, round . sin $ pi / 2 * x)) [0 .. 3]
          length = concatMap (replicate 2) [1..]
          path = concat $ zipWith (\ x y -> replicate x y) length dirs

add :: Num a => (a,a) -> (a,a) -> (a,a)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

manhattanNorm :: Num a => (a,a) -> a
manhattanNorm (x,y) = abs x + abs y