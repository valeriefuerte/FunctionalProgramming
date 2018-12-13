module FP4 where

import Data.Maybe
import Data.List
import Data.Char

{-1.1-}
id_ :: a -> a
id_ x = x
{-1.2-}
eval :: (a -> b, a) -> b
eval (g, x) = g x
{-1.3-}
exchange :: (a, b) -> (b, a)
exchange (a, b) = (b, a)
{-1.4-}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g h = f(g h)
{-1.5-}
curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f a b = f (a, b)
{-1.6-}
associate :: (a, (b, c)) -> ((a, b), c)
associate (a, (b, c)) = ((a, b), c)

{-2-}
minMax :: Ord a => [a] -> Maybe (a, a)
minMax xs = foldr go id xs Nothing where
  go x r Nothing = r (Just (x, x))
  go x r mnmx@(Just (minimum, maximum))
    | x < minimum = r (Just (x, maximum))
    | maximum < x = r (Just (minimum, x))
    | otherwise = r mnmx

{-3-}
size :: Integer -> Integer
size x = let y = abs(x) `div` 10
    in if y == 0 then 1 else size(y) + 1
miterate :: (a -> Maybe (a, b)) -> a -> [b]
miterate f = go . f where
    go Nothing = []
    go (Just (x, y)) = y : (go (f x))
sumd :: Integer -> Integer
sumd = sum . miterate f where
    f 0 = Nothing
    f x = Just (x `divMod` 10)

{-4-}
major_item :: Eq a => [a] -> Maybe(a) -> Int -> Maybe(a)
major_item x cand conf =
    if null x
        then if conf > 0
            then cand
            else Nothing
        else if conf == 0
            then major_item (tail x) (Just (head x)) 1
            else if cand == Just(head x)
                then major_item (tail x) cand (conf+1)
                else major_item (tail x) cand (conf-1)

majority :: Eq a => [a] -> Maybe(a)
majority x = major_item x Nothing 0

{-5-}
f :: (a -> a) -> Int -> (a -> a)
f g n 
    | n > 1 = \x -> (f g (n-1)) $ g x
    | n == 1 = \x -> g x
    | otherwise = error "n must be positive number"

{-6-}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci(n-2) + fibonacci(n-1)) `mod` 10

{-7-}
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if toUpper(x) /= toUpper(last xs)
        then False
        else isPalindrome(init xs)
