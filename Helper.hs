import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Function
import Data.Ord

p10 = 6469693230
p10' = snd $ reduce_2_3 p10

reduce_2 x
    | odd x     = (0, x)
    | otherwise = first succ $ reduce_2 (x `div` 2)

reduce_3 x
    | x `mod` 3 /= 0 = (0, x)
    | otherwise      = first succ $ reduce_3 (x `div` 3)

reduce_2_3 = runState $ do a <- state reduce_2
                           b <- state reduce_3
                           return (a, b)

-- | add the input to each coefficient
iter_reduce coefs = minimumBy (comparing (snd . fst)) .
                    map (reduce_2_3 &&& id) .
                    sequence (map (\x y -> abs (x + y)) coefs)

iter_pm1 = iter_reduce [1, -1]

iter_pm2a3b x =
    let twos   = takeWhile (<=2*x) $ iterate (*2) 1
        threes = takeWhile (<=3*x) $ iterate (*3) 1
        coefs  = filter (<=2*3*x) $ (*) <$> twos <*> threes
        coefs' = [negate, id] <*> coefs
    in  iter_reduce coefs' x

process f 1 = []
process f x = let ((coef, x'), y) = f x
              in  (coef, x', y) : process f x'

        