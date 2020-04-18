{-# LANGUAGE OverloadedStrings #-}

module REVC
  (
    revcMain
  ) where

import Utils
import Data.List.Split (splitOn)

revcMain = wrapSolver "revc" revc

complement x
    | x == 'A' = 'T'
    | x == 'C' = 'G'
    | x == 'G' = 'C'
    | otherwise = 'A'

-- head is unsafe but we still use it.

revc = reverse . map complement . head . splitOn "\n"  
  
    
 
