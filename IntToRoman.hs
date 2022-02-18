module IntToRoman
  ( intToRoman
  ) where
import           Control.Monad                  ( (<=<)
                                                , (>=>)
                                                )
import           GHC.Float                      ( int2Float )
import           Validate

numToChar :: Int -> Validate [String] Char
numToChar 1000 = Success 'M'
numToChar 500  = Success 'D'
numToChar 100  = Success 'C'
numToChar 50   = Success 'L'
numToChar 10   = Success 'X'
numToChar 5    = Success 'V'
numToChar 1    = Success 'I'
numToChar c    = Failure [show c ++ " has no numeral"]

intToRoman :: Int -> Validate [String] String
intToRoman =
  (mapM numToChar . concat . reverse)
    <=< (traverse (romanForm . (`mod` 10)) . expand)
    <=< inRange 0 4000

romanForm :: Int -> Validate [String] [Int]
romanForm n = form n
 where
form :: Int -> Validate [String] [Int]
form 0 = Success []
form x = case x of
  1  -> Success [1]
  2  -> Success [1, 1]
  3  -> Success [1, 1, 1]
  4  -> Success [1, 5]
  5  -> Success [5]
  6  -> Success [5, 1]
  7  -> Success [5, 1, 1]
  8  -> Success [5, 1, 1, 1]
  9  -> Success [1, 10]
  10 -> Success [10]
  _  -> Failure ["L + Ratio Bozo"]

expand :: Int -> [Int]
expand = reverse . zipWith ($) [ (* (10 ^ x)) | x <- [0 ..] ] . expand'
 where
  expand' :: Int -> [Int]
  expand' 0 = []
  expand' n = (n `mod` 10) : expand' (div n 10)

inRange :: Ord a => a -> a -> a -> Validate [String] a
inRange min max = fromPred ["Value not in range"] (\n -> min < n && n > max)
--fmap ((10 ^ floor (logBase 10 (int2Float n))) *)
