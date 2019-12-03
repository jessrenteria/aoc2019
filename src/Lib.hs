module Lib
  ( splitOn
  ) where

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs =
  case dropWhile p xs of
    [] -> []
    xs' -> run : splitOn p xs''
      where
        (run, xs'') = break p xs'
