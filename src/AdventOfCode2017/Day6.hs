module AdventOfCode2017.Day6 where


import qualified Data.List.Zipper as LZ
import           Data.List.Zipper (Zipper)
import qualified Data.List        as L

next :: Zipper a -> Zipper a
next lz =
    if LZ.endp lz
    then LZ.start lz
    else LZ.right lz

maximum' :: (Eq a, Ord a) => Zipper a -> a
maximum' zip = LZ.foldlz m (LZ.cursor zip) (LZ.start zip)
    where m a z = if a < LZ.cursor z
                  then LZ.cursor z
                  else a

setCursorToMaximum :: (Eq a, Ord a) => Zipper a -> Zipper a
setCursorToMaximum zip = setCursorToMaximum' z
    where z = LZ.start zip
          m = maximum' zip
          setCursorToMaximum' z' =
            if LZ.cursor z' == m
            then z'
            else setCursorToMaximum' . next $ z'

redistribute :: Zipper Int -> Zipper Int
redistribute zip = iterate' d (next . addOne) z'
    where z = setCursorToMaximum zip
          d = LZ.cursor z
          z' = next $ LZ.replace 0 z



addOne :: (Num a) => Zipper a -> Zipper a
addOne zip = LZ.replace (LZ.cursor zip + 1)  zip

iterate' :: Int -> (Zipper Int -> Zipper Int) -> Zipper Int ->  Zipper Int
iterate' n f z
    | n > 0  = iterate' (n - 1) f (f z)
    | n == 0 = z
    | otherwise = undefined