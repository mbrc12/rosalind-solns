{-# LANGUAGE OverloadedStrings #-}

module GC  (
    gcMain
  ) where

import Utils
import Data.Map.Strict (foldlWithKey, findMin)

gcMain = wrapSolver "gc" gc

gcContent :: DNA -> Float
gcContent dna = let
  count eq = length . filter eq 
  gc = count (\x -> x == 'G' || x == 'C') dna
  total = length dna
  in fromIntegral gc / fromIntegral total

maxPair :: (Ord k, Ord v) => Map k v -> (k, v)
maxPair mp = foldlWithKey (\(ck, cv) nk nv ->
                         if nv > cv
                         then (nk, nv)
                         else (ck, cv))
             (findMin mp) mp

gc = showPair . maxPair . fmap gcContent . fastaReader
  where showPair (name, val) = tail name <|> show (100 * val)

