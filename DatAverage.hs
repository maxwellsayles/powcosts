{-
Given a .dat as input, computes the average of the y.
-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment (getArgs)
import System.FilePath.Glob (glob)
import System.FilePath.Posix (takeBaseName)
import Text.Printf (printf)

readDat :: FilePath -> IO [(Int, Double)]
readDat filename = do
  xs <- lines <$> readFile filename
  return $ map (read :: String -> (Int, Double)) $
         map (\s -> "(" ++ s ++ ")") xs
         

doit args = do
  let filename = head args
  ys <- map snd <$> readDat filename
  printf "%.5f\n" $ (sum ys) / (1000 * (fromIntegral (length ys)))

usage = printf "Usage: DatAverage.hs <dat-file>\n\n"

main = do
  args <- getArgs
  if null args then usage else doit args
