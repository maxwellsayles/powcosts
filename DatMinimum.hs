{-
Given many .dat files as input, this computes the smallest y for each x
and prints the file associated with the minimum.
-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function (on)
import Data.List (group, minimumBy, sort)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs $ transpose xss

readDat :: FilePath -> IO [(FilePath, Double)]
readDat f = ((f, 1.0) :) .
            tail .
            map (first (const base)) .
            map (read :: String -> (Int, Double)) .
            map (\s -> "(" ++ s ++ ")") .
            lines <$>
            readFile f
    where base = takeBaseName f

doit patterns = do
  in_names <- concat <$> mapM glob patterns
  putStrLn "Processing files:"
  mapM_ print in_names
  putStrLn ""

  in_data <- forM in_names readDat

  -- Compute the fastest time for each row of all methods.
  let rows = tail $ transpose in_data
  let hist = map (head &&& length) $
             group $
             sort $
             map (fst . minimumBy (comparing snd)) rows
  mapM_ print hist

usage = do
  putStrLn "Usage: DatMinimum.hs file1.dat file2.dat ..."
  putStrLn "  Outputs the number of times each file contains a minimum."
  putStrLn ""
                         
main = do
  args <- getArgs
  if null args then usage else doit args
