module AdventOfCode2017.Day4 where

import qualified Data.List as L

isValidPassword :: String -> Bool
isValidPassword = areUnique . words
    where areUnique xs = case xs of
            [] -> True
            (x:rest) -> notElem x rest && areUnique rest

isAnagram :: String -> String -> Bool
isAnagram [] _ = False
isAnagram _ [] = False
isAnagram xs ys
    | length xs /= length ys = False
    | otherwise = foldl1 (&&) $ zipWith (==) (L.sort xs) (L.sort ys)


isValidStrongPassword :: String -> Bool
isValidStrongPassword str = isValidStrongPassword' . words $ str
    where isValidStrongPassword' xs = case xs of
            [] -> True
            (x:rest) -> (not $ any (isAnagram x) rest) && isValidStrongPassword' rest
