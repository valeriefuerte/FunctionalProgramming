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

main = putStrLn "Everything is okey!"