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
sum :: Integer -> Integer -> Integer
sum 0 = 0
sum x = (x `mod` 10) + sum (x `div` 10)
size :: Integer -> Integer -> Integer
size n | (n<10) = 1
       | otherwise = 1 + size (n `div` 10) 10

{-5-}
f :: (a -> a) -> Int -> (a -> a)
ntimes f g n
    | n <= 0     = error "n must be positive number"
    | otherwise = ntimes g (f g (n - 1))

{-6-}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
lastDigit :: Integer->Integer
lastDigit n = mod (fibonacci n) 10