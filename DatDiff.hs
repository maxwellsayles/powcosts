{-
Given two dat files as input, outputs two dat files 
where the first and second output correspond to the first
and second input respectively, but with the common lines between
the two remove.
-}
import Control.Applicative
import Control.Arrow
import System.Environment (getArgs)

doit args = do
  let [infile1, infile2, outfile1, outfile2] = args
  xs <- lines <$> readFile infile1
  ys <- lines <$> readFile infile2
  let (as, bs) = unzip $
                 map snd $
                 filter fst $
                 zipWith (\a b -> (a /= b, (a, b))) xs ys
  print $ length $ filter id $ zipWith (<) as bs
  print $ length $ filter id $ zipWith (>) as bs
  writeFile outfile1 $ unlines as
  writeFile outfile2 $ unlines bs

usage = do
  putStrLn "Usage: DatDiff.hs infile1 infile2 outfile1 outfile2"
  putStrLn ""
                         
main = do
  args <- getArgs
  if length args /= 4 then usage else doit args
  