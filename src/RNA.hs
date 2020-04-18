module RNA
  (
    rnaMain
  ) where

import Utils

rnaMain = wrapSolver "rna" rna

rna = map (\bp -> if bp == 'T' then 'U' else bp)

    
 
