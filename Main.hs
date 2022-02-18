module Main where
import           Control.Monad                  ( (<=<)
                                                , (>=>)
                                                , join
                                                , mfilter
                                                )
import           Data.List                      ( group
                                                , nub
                                                , sort
                                                )
import           IntToRoman                     ( intToRoman )
import           Parser                         ( runSections )
import           RomanToInt                     ( romanToInt )
import           Sample                         ( sample )
import           Validate

f = validate

main :: IO ()
main = do
  putStr "Input Roman: "
  s <- getLine
  putStrLn $ validate show show (romanToInt s)
  main
