module FIBD
  (
    fibdMain
  ) where

import Utils
import Data.List.Split (splitOn)

fibdMain = wrapSolver "fibd" fibd

evolve :: Int -> [Int] -> [Int]
evolve m pop = let
  newborn = sum $ tail pop
  ages = newborn : pop
  in take m ages


parseInts :: String -> [Int]
parseInts = map read . splitOn " "     

fibd input = let
  [n, m] = parseInts $ head $ splitOn "\n" input
  pop = sum <$> iterate (evolve m) [1] -- one of age 0
  in show $ pop !! (n - 1)
