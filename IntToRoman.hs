module IntToRoman
  ( intToRoman
  ) where
import           Control.Monad                  ( (<=<)
                                                , (>=>)
                                                )
import           Validate

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
intToRoman =
  mapM romanForm
    .   expand'
    >=> (mapM numToChar . concat . reverse . zipWith
          ($)
          [ fmap ((10 ^ n) *) | n <- [0 ..] ]
        )


 where
  expand' :: Int -> [Int]
  expand' 0 = []
  expand' n = (n `mod` 10) : expand' (div n 10)

  romanForm :: Int -> Maybe [Int]
  romanForm 0  = Just []
  romanForm 1  = Just [1]
  romanForm 2  = Just [1, 1]
  romanForm 3  = Just [1, 1, 1]
  romanForm 4  = Just [1, 5]
  romanForm 5  = Just [5]
  romanForm 6  = Just [5, 1]
  romanForm 7  = Just [5, 1, 1]
  romanForm 8  = Just [5, 1, 1, 1]
  romanForm 9  = Just [1, 10]
  romanForm 10 = Just [10]
  romanForm _  = Nothing

inRange :: Ord a => a -> a -> a -> Validate [String] a
inRange min max = fromPred ["Value not in range"] (\n -> min < n && n > max)
