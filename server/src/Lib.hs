module Lib where

padLeft :: a -> Int -> [a] -> [a]
padLeft x i xs
    | length xs >= i = xs
    | otherwise = padLeft x i (x:xs)


