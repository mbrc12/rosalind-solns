{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils
  (
    wrapSolver,
    (<->),
    (<|>),
    initArray,
    (<?>),
    (<=>),
    Map,
    fastaReader,
    DNA(..)
  ) where

import System.IO
import System.Environment
import System.IO.Unsafe
import Data.Array
import Data.Map.Strict (
  Map(..),
  fromList,
  lookup
  )
import Data.List.Split (splitWhen)

(</>) :: FilePath -> FilePath -> FilePath
(</>) base extra = base ++ "/" ++ extra

(<->) :: String -> String -> String
(<->) a b = a ++ " " ++ b

(<|>) :: String -> String -> String
(<|>) a b = a ++ "\n" ++ b


basePath :: IO FilePath
basePath = do
  home <- getEnv "HOME"
  return $ home </> "Data" </> "rosalind"

extendBase, getInputPath, getOutputPath :: String -> IO String
extendBase extra = pure (</> extra) <*> basePath

getInputPath name = extendBase $ name </> "input"
getOutputPath name = extendBase $ name </> "output"

readInput :: String -> IO String
readInput name = do
  path <- getInputPath name
  readFile path

writeOutput :: String -> String -> IO ()
writeOutput name output = do
  path <- getOutputPath name
  writeFile path output

wrapSolver :: String -> (String -> String) -> IO ()
wrapSolver name solver =
  readInput name >>= return . solver >>= writeOutput name

--- Array functions

initArray :: Ix i => (i, i) -> (i -> v) -> Array i v
initArray bounds f = array bounds $ map (\i -> (i, f i)) $ range bounds

-- indexing operator rename
(<?>) = (!)

-- set an element
arr <=> (i, e) = arr // [(i, e)]

-- FASTA convenience functions
type DNA = String

isFastaId :: String -> Bool
isFastaId line = length line > 0 && head line == '>'

fastaReader :: String -> Map String DNA 
fastaReader fasta = let
  lines_ = lines fasta
  ids = filter isFastaId lines_
  seqs = tail $ map concat $ splitWhen isFastaId lines_
  in fromList $ zip ids seqs 
