module Main where
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

charToNum :: Char -> Maybe Int
charToNum 'M' = Just 1000
charToNum 'D' = Just 500
charToNum 'C' = Just 100
charToNum 'L' = Just 50
charToNum 'X' = Just 10
charToNum 'V' = Just 5
charToNum 'I' = Just 1
charToNum _   = Nothing

romanToInt :: String -> Maybe Int
romanToInt =
  fmap (foldr1 (\a b -> if a >= b then a + b else b - a) . sumCon)
    .   validate
    <=< mapM charToNum

sumCon :: [Int] -> [Int]
sumCon []       = []
sumCon (x : xs) = sum (takeWhile (== x) xs) + x : sumCon (dropWhile (== x) xs)

validate :: [Int] -> Maybe [Int]
validate xs = runSections xs *> Just xs

main :: IO ()
main = do
  s <- getLine
  print $ romanToInt s
  main
