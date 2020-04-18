module IPRB
  (
    iprbMain
  ) where

import Utils
import Data.List.Split (splitOn)

iprbMain = wrapSolver "iprb" iprb

parseInts :: String -> [Int]
parseInts = map read . splitOn " "     

getProb :: Int -> Int -> Int -> Float
getProb _yy _yY _YY = let
  [_xx, _xX, _XX] = map fromIntegral [_yy, _yY, _YY]
  n = _xx + _xX + _XX
  all = n * (n - 1) / 2
  onexxonexX = _xx * _xX / 2
  bothxx = _xx * (_xx - 1) / 2
  bothxX = _xX * (_xX - 1) / 8
  in 1.0 - (onexxonexX + bothxx + bothxX) / all
  

iprb input = let
  [nYY, nyY, nyy] = parseInts $ head $ splitOn "\n" input
  in show $ getProb nyy nyY nYY
