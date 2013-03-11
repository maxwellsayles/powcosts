{-
Generates representations for n.
-}

import Data.Array
import Data.List
import Text.Printf

data Term = Term { termSquares :: {-# UNPACK #-} !Int,
                   termCubes   :: {-# UNPACK #-} !Int,
                   termSign    :: {-# UNPACK #-} !Int }
            deriving (Eq)

instance Show Term where
    show (Term a b s) = printf " %c2^%d*3^%d" sign a b
        where sign = if s == -1 then '-' else '+'

data Rep = Rep { repLength :: {-# UNPACK #-} !Int,
                 repTerms  :: [Term] }
           deriving (Eq)

instance Show Rep where
    show (Rep _ terms) = concatMap show terms

instance Ord Rep where
    (Rep l1 _) `compare` (Rep l2 _) = l1 `compare` l2
         
n = 23814216 :: Int

one = Term 0 0 0

neg (Term a b s) = Term a b s'
    where s' = if s < 0 then 0 else -1

square a' (Rep l ts) = Rep l $ map (\(Term a b s) -> Term (a + a') b s) ts

cube b' (Rep l ts) = Rep l $ map (\(Term a b s) -> Term a (b + b') s) ts

add t (Rep l ts) = Rep (l + 1) (t : ts)

sub t (Rep l ts) = Rep (l + 1) (neg t : ts)

addChain :: Array Int Rep
addChain = opt where
    opt = listArray (0, n) $
               (Rep 0 []) :
               (Rep 1 [Term 0 0 0]) :
               [f i | i <- [2..n]]
    f i | m == 0 = let x = square 1 $ opt ! (i `div` 2)
                       y = cube 1 $ opt ! (i `div` 3)
                   in  min x y
        | m == 1 = add one $ opt ! (i - 1)
        | m == 2 = square 1 $ opt ! (i `div` 2)
        | m == 3 = let x = cube 1 $ opt ! (i `div` 3)
                       y = add one $ square 1 $ opt ! ((i - 1) `div` 2)
                   in  min x y
        | m == 4 = let x = square 1 $ opt ! (i `div` 2)
                       y = add one $ cube 1 $ opt ! ((i - 1) `div` 3)
                   in  min x y
        | m == 5 = add one $ square 1 $ opt ! ((i - 1) `div` 2)
        where m = i `mod` 6

memoChain :: Array Int Rep
memoChain = opt where
    opt = listArray (0, n) $
                (Rep 0 []) :
                (Rep 1 [Term 0 0 0]) :
                [f i | i <- [2..n]]
    f i | m == 0 = let x = square 1 $ opt ! (i `div` 2)
                       y = cube 1 $ opt ! (i `div` 3)
                   in  min x y
        | m == 1 = let x = add one $ opt ! (i - 1)
                       y = sub one $ square 1 $ opt ! ((i + 1) `div` 2)
                   in  min x y
        | m == 2 = let x = square 1 $ opt ! (i `div` 2)
                       y = sub one $ cube 1 $ opt ! ((i + 1) `div` 3)
                   in  min x y
        | m == 3 = let x = cube 1 $ opt ! (i `div` 3)
                       y = add one $ square 1 $ opt ! ((i - 1) `div` 2)
                       z = sub one $ square 1 $ opt ! ((i + 1) `div` 2)
                   in  minimum [x, y, z]
        | m == 4 = let x = square 1 $ opt ! (i `div` 2)
                       y = add one $ cube 1 $ opt ! ((i - 1) `div` 3)
                   in  min x y
        | m == 5 = let x = add one $ square 1 $ opt ! ((i - 1) `div` 2)
                       y = sub one $ opt ! (i + 1)
                   in  min x y
        where m = i `mod` 6

dbnsR2l 1 = Rep 1 [Term 0 0 0]
dbnsR2l 2 = Rep 1 [Term 1 0 0]
dbnsR2l 3 = Rep 1 [Term 0 1 0]
dbnsR2l n
    | m == 0 || m == 2 || m == 4 = square 1 $ dbnsR2l (n `div` 2)
    | m == 3 = cube 1 $ dbnsR2l (n `div` 3)
    | m == 1 = add one $ dbnsR2l (n - 1)
    | m == 5 = sub one $ dbnsR2l (n + 1)
    where m = n `mod` 6

main = do
  print $ dbnsR2l n
--  print $ addChain ! n
--  print $ memoChain ! n


