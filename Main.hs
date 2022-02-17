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

main :: IO ()
main = do
  putStr "Input Roman: "
  s <- getLine
  putStrLn $ maybe "Invalid" show $ romanToInt s
  main
