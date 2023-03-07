module Utils (
    orElse,
    update,
    untilFixedPoint
) where

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing  x = x

update :: (Eq a) => (m -> m -> m) -> [(a, m)] -> (a, m) -> [(a, m)]
update _ []           (a,m) = [(a,m)]
update f ((a1,m1):xs) (a,m) = if a == a1 then (a1, f m1 m) : xs else (a1,m1) : update f xs (a,m)

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f x = let y = f x in if y == x then y else untilFixedPoint f y
