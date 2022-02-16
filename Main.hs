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
import           Parser                         ( runSections )
import           Sample                         ( sample )
import           RomanToInt                     (romanToInt)
import           IntToRoman                     (intToRoman)

main :: IO ()
main = do
  putStr $ "Input Roman: "
  s <- getLine
  putStrLn $ maybe ("Invalid") show $ romanToInt s
  main
