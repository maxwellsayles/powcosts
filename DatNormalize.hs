{-
Given many .dat files as input, this computes the smallest y for each x
and then normalizes each y relative to the fastest y.
-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment (getArgs)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs $ transpose xss



outfilename :: FilePath -> FilePath
outfilename filename = helper filename
    where helper "" = ""
          helper ".dat" = "_norm.dat"
          helper (x:xs) = x : helper xs

readDat :: FilePath -> IO [Double]
readDat f = (1.0 :) .
            tail .
            map snd .
            map (read :: String -> (Int, Double)) .
            map (\s -> "(" ++ s ++ ")") .
            lines <$>
            readFile f

doit patterns = do
  in_names <- concat <$> mapM glob patterns
  putStrLn "Processing files:"
  mapM_ print in_names
  putStrLn ""

  in_data <- forM in_names readDat

  -- Compute the fastest time for each row of all methods.
  let rows = transpose in_data
  let mins = 1.0 : (tail $ map minimum rows)

  -- Normalize all times relative to the fastest.
  let normalize m = map (/ m)
  let out_data = transpose $ zipWith normalize mins rows

--  let out_names = map outfilename in_names
  forM_ (zip in_names out_data) $ \(name, xs) -> do
                  let avg = (sum xs) / (fromIntegral $ length xs)
                  printf "Normalized Average for %s: %.5f\n"
                             (show $ takeBaseName name) avg

--                  let xs' = map (\(x, y) -> show x ++ ", " ++ show y) $
--                            zip [1..] xs
--                  writeFile out_name $ unlines xs'

usage = do
  putStrLn "Usage: DatDiff.hs file1.dat file2.dat ..."
  putStrLn "  This creates file1_sub.dat file2_sub.dat ..."
  putStrLn "  Each line output will be unique from every file."
  putStrLn ""
                         
main = do
  args <- getArgs
  if null args then usage else doit args
