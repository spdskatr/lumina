{- For when your really need some debugging. -}

module Lumina.UnsafeUtils (unsafeTagLengthList, unsafeTag) where

import System.IO.Unsafe (unsafePerformIO)

unsafeTagLengthList :: [a] -> [a]
unsafeTagLengthList l = unsafePerformIO $ do
    putStrLn $ show $ length l
    return l

unsafeTag :: (Show a) => a -> a
unsafeTag x = unsafePerformIO $ do
    putStrLn $ show x
    return x