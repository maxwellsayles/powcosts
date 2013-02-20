{-
Given two dat files as input, outputs two dat files 
where the first and second output correspond to the first
and second input respectively, but with the common lines between
the two remove.
-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment (getArgs)
import System.FilePath.Glob (glob)

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs $ transpose xss

uniqueLines :: Ord a => [a] -> [[a]] -> [a]
uniqueLines xs yss = map snd $
                     filter fst $
                     zipWith (\x ys -> (unique x ys, x)) xs yss'
    where yss' = transpose yss
          unique x ys = all (/=x) ys

outfilename :: FilePath -> FilePath
outfilename filename = helper filename
    where helper "" = ""
          helper ".dat" = "_sub.dat"
          helper (x:xs) = x : helper xs

{- |
For each x in xs, generate (x, delete x xs).
This function is a faster version of
  [(x, delete x xs) | x <- xs]
-}
pairsWithout :: [a] -> [(a, [a])]
pairsWithout xs = [helper i | i <- [1..length xs]]
    where helper i = (\ys -> (xs !! (i - 1), ys)) $
                     uncurry (++) $
                     first init $
                     splitAt i xs

doit patterns = do
  in_names <- concat <$> mapM glob patterns
  putStrLn "Processing files:"
  mapM_ print in_names
  putStrLn ""

  xss <- mapM (\f -> lines <$> readFile f) in_names
  let xss' = map (uncurry uniqueLines) $ pairsWithout xss
  let out_names = map outfilename in_names
  forM_ (zip out_names xss') $ \(out_name, xs) -> do
                  putStrLn $ "Writing " ++ out_name
                  writeFile out_name $ unlines xs

usage = do
  putStrLn "Usage: DatDiff.hs file1.dat file2.dat ..."
  putStrLn "  This creates file1_sub.dat file2_sub.dat ..."
  putStrLn "  Each line output will be unique from every file."
  putStrLn ""
                         
main = do
  args <- getArgs
  if null args then usage else doit args
