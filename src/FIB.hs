{-# LANGUAGE OverloadedStrings #-}

module FIB
  (
    fibMain
    -- calc
  ) where

import Utils
import Data.List.Split (splitOn)

fibMain = wrapSolver "fib" fib

parseInts :: String -> [Int]
parseInts = map read . splitOn " "     

calc n k = (\(a, b) -> a + b) $ dp <?> (n - 1)
  where
    dp = initArray (0, n) solve
    solve i
      | i == 0 = (1, 0)
      | otherwise = let
          (born, old) = dp <?> (i - 1)
          in (old * k, born + old)

fib input = let
  [n, k] = parseInts $ head $ splitOn "\n" input
  ans = calc n k
  in show ans
  
    
