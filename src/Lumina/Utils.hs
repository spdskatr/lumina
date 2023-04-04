module Lumina.Utils (
    orElse,
    update,
    untilFixedPoint,
    countUp,
    hasDuplicates,
    findDuplicateGroups,
    internalError,
    parseError,
    fastNub,
    headMaybe,
    indent
) where

import Data.List (group, sort, groupBy)
import Data.Function (on)

fastNub :: (Ord a) => [a] -> [a]
fastNub = map head . group . sort

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing  x = x

update :: (Eq a) => (m -> m -> m) -> [(a, m)] -> (a, m) -> [(a, m)]
update _ []           (a,m) = [(a,m)]
update f ((a1,m1):xs) (a,m) = if a == a1 then (a1, f m1 m) : xs else (a1,m1) : update f xs (a,m)

hasDuplicates :: (Ord a) => [(a, m)] -> Bool
hasDuplicates l = let ks = sort $ map fst l in any (uncurry (==)) $ zip ks $ drop 1 ks

findDuplicateGroups :: (Ord a, Ord m) => [(a,m)] -> [[(a,m)]]
findDuplicateGroups l = filter (\x -> length x > 1) $ groupBy ((==) `on` fst) $ sort l

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f x = let y = f x in if y == x then y else untilFixedPoint f y

countUp :: [a] -> [(Int, a)]
countUp = zip [0..]

internalError :: String -> a
internalError msg = error ("Internal compiler error: " ++ msg)

parseError :: String -> a
parseError msg = error ("Parse error: " ++ msg)

indent :: String -> String
indent ex = unlines (("  " ++) <$> lines ex)