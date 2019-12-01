module Main where

import Lib

main :: IO ()
main = putStrLn . show =<< solve "data/day1.txt" parseDay1 solveDay1

solve :: (Show b) => String -> (String -> a) -> (a -> b) -> IO b
solve filepath parser solver = do
  input <- readFile filepath
  return $ solver $ parser input

parseDay1 :: String -> [Int]
parseDay1 = map read . lines

solveDay1 :: [Int] -> Int
solveDay1 = sum . map massToFuel
  where
    massToFuel m = (m `div` 3) - 2
