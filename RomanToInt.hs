module RomanToInt
  ( romanToInt
  ) where
import           Control.Monad                  ( (<=<)
                                                , join
                                                , mfilter
                                                )
import           Data.List                      ( group
                                                , nub
                                                , sort
                                                )
import           Parser                         ( runSections )
import           Sample                         ( sample )
import           Validate

charToNum :: Char -> Validate [String] Int
charToNum 'M' = Success 1000
charToNum 'D' = Success 500
charToNum 'C' = Success 100
charToNum 'L' = Success 50
charToNum 'X' = Success 10
charToNum 'V' = Success 5
charToNum 'I' = Success 1
charToNum _   = Failure ["Invalid Charecter"]

romanToInt :: String -> Validate [String] Int
romanToInt =
  fmap (foldr1 (\a b -> if a >= b then a + b else b - a) . sumCon)
    .   valid
    <=< mapM charToNum

sumCon :: [Int] -> [Int]
sumCon []       = []
sumCon (x : xs) = sum (takeWhile (== x) xs) + x : sumCon (dropWhile (== x) xs)

valid :: [Int] -> Validate [String] [Int]
valid xs = runSections xs *> Success xs
