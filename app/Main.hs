module Main where

import Lib

main :: IO ()
main = putStrLn . show =<< solve "data/day1.txt" parseDay1 solveNext1

solve :: (Show b) => String -> (String -> a) -> (a -> b) -> IO b
solve filepath parser solver = do
  input <- readFile filepath
  return $ solver $ parser input

parseDay1 :: String -> [Int]
parseDay1 = map read . lines

massToFuel :: Int -> Int
massToFuel m = (m `div` 3) - 2

solveDay1 :: [Int] -> Int
solveDay1 = sum . map massToFuel

solveNext1 :: [Int] -> Int
solveNext1 = sum . map massToFuelToFuel
  where
    massToFuelToFuel = sum . takeWhile (> 0) . drop 1 . iterate massToFuel
