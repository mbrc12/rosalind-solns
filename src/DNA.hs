module DNA
  (
    dnaMain
  ) where

import Utils

name = "dna"

dnaMain = wrapSolver name dna

dna gene = count 'A' <-> count 'C' <-> count 'G' <-> count 'T' <|> ""
  where
    count c = show $ length $ filter (== c) gene

 
