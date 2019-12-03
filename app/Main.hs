{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception
import Control.Monad.ST
import Data.Array.IArray as IArray
import Data.Array.MArray as MArray
import Data.Array.ST as STArray
import Data.Either
import System.IO.Unsafe

import Lib

main :: IO ()
main = putStrLn . show =<< solve "data/day2.txt" parseDay2 solveNext2

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

parseDay2 :: String -> [Int]
parseDay2 = map read . splitOn (== ',')

listToArray :: (MArray.MArray a e m) => [e] -> m (a Int e)
listToArray xs = MArray.newListArray (0, length xs) xs

handleCode :: (MArray.MArray a Int m) => Int -> a Int Int -> m ()
handleCode instrPtr arr = do
  opCode <- MArray.readArray arr instrPtr
  case opCode of
    1 -> handleBinaryOp (+) instrPtr arr
    2 -> handleBinaryOp (*) instrPtr arr
    99 -> return ()

handleBinaryOp :: (MArray.MArray a Int m) => (Int -> Int -> Int) -> Int -> a Int Int -> m ()
handleBinaryOp op instrPtr arr = do
  xIdx <- MArray.readArray arr (instrPtr + 1)
  x <- MArray.readArray arr xIdx
  yIdx <- MArray.readArray arr (instrPtr + 2)
  y <- MArray.readArray arr yIdx
  zIdx <- MArray.readArray arr (instrPtr + 3)
  MArray.writeArray arr zIdx (x `op` y)
  handleCode (instrPtr + 4) arr

runIntcode :: [Int] -> (Int, Int) -> Int
runIntcode xs (y, z) = runST $ do
  arr <- listToArray xs :: ST s (STArray.STUArray s Int Int)
  MArray.writeArray arr 1 y
  MArray.writeArray arr 2 z
  handleCode 0 arr
  MArray.readArray arr 0

solveDay2 :: [Int] -> Int
solveDay2 xs = runIntcode xs (12, 2)

-- Disgusting and shameful but quick to implement.
-- TODO: explore properly handling failure in runIntcode (with an Either?).
solveNext2 :: [Int] -> (Int, Int)
solveNext2 xs = fst $ head $ filter ((== 19690720) . snd) $ rights $ unsafePerformIO $ mapM run candidates
  where
    candidates = [ (y, z) | y <- [0..99], z <- [0..99] ]
    run :: (Int, Int) -> IO (Either SomeException ((Int, Int), Int))
    run candidate = try $ evaluate $ (candidate, runIntcode xs candidate)
