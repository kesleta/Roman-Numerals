module IntToRoman (intToRoman) where

numToChar :: Int -> Maybe Char
numToChar 1000 = Just 'M'
numToChar 500  = Just 'D'
numToChar 100  = Just 'C'
numToChar 50   = Just 'L'
numToChar 10   = Just 'X'
numToChar 5    = Just 'V'
numToChar 1    = Just 'I'
numToChar _    = Nothing

intToRoman :: Int -> Maybe String
intToRoman = mapM numToChar . concat . reverse . (zipWith ($) [fmap ((10^n)*) | n <- [0..]]) .  fmap romanForm . expand'
  where
    expand' :: Int -> [Int]
    expand' 0 = []
    expand' n = (n `mod` 10) : (expand' $ div n 10)

    romanForm :: Int -> [Int]
    romanForm 0  = []
    romanForm 1  = [1]
    romanForm 2  = [1, 1]
    romanForm 3  = [1, 1, 1]
    romanForm 4  = [1, 5]
    romanForm 5  = [5]
    romanForm 6  = [5, 1]
    romanForm 7  = [5, 1, 1]
    romanForm 8  = [5, 1, 1, 1]
    romanForm 9  = [1, 10]
    romanForm 10 = [10]