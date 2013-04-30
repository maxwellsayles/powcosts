import Control.Applicative

loadTimes :: FilePath -> IO [Double]
loadTimes filename = do
  xs <- lines <$> readFile filename
  return $ map (!!1) $
         map (read :: String -> [Double]) $
         map (\s -> "[" ++ s ++ "]") xs

compareTimes f1 f2 = do
  xs1 <- loadTimes f1
  xs2 <- loadTimes f2
  return $ length $ filter (> 0) $ zipWith (-) xs1 xs2