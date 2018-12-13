module FP5 where

import Data.List
import Data.Char
import Data.Monoid
import Data.Foldable
import Data.Semigroup

{-1-}
circShiftL :: Int -> [a] -> [a]
circShiftL step list = let step_mod = mod step (length list)
    in drop step_mod list ++ take step_mod list

{-2-}
indices :: [a] -> [(Integer, a)]
indices l = zip [0..] l

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy list predicate = map (\a -> if predicate a then mempty else a) list

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum l1 l2 l3 = zipWith (+) l1 (zipWith (+) l2 l3)

{-3-}
revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun
fun (first, last)
    | first > last = Nothing
    | last == minBound = Just (last, (succ first, last))
    | otherwise = Just (last, (first, pred last))

{-4-}
seriesK :: Int -> [Rational]
seriesK k = let r_k = toRational k in knot (r_k, 0)

knot :: (Rational, Int) -> [Rational]
knot (x, s)  = 1 / y : result_list
    where y = x ^ s
          result_list = knot (x, s + 1)

{-5-}
newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x  = y : merge (x:xs) ys 
                    | otherwise = x : merge xs (y:ys) 
concatList :: Ord a => SortedList a -> SortedList a -> SortedList a

concatList a b = SortedList (merge (getSorted a) (getSorted b))

instance Ord a => Semigroup (SortedList a) where
    (<>) = concatList

instance Ord a => Monoid (SortedList a) where
    mempty = SortedList []

{-6-}
slice :: [a] -> ([a],[a]) 
slice a = (take mid a, drop mid a) 
      where mid = length a `div` 2 

msort :: Ord a => [a] -> SortedList a 
msort [] = mempty
msort [x] = SortedList [x] 
msort xs = mappend (msort left) (msort right) 
      where (left,right) = slice xs
