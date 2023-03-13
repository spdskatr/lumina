{-# OPTIONS_GHC -Wno-orphans #-}
module ParserGenTest (runPGTest) where

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, quickCheckWith, stdArgs)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.ParserGen (Terminal(..), fromInt, toInt)
import Test.QuickCheck.Test (Args(..))

instance Arbitrary TokenTag where
    arbitrary = arbitraryBoundedEnum

test_TerminalSerdeAreDuals :: TokenTag -> Bool
test_TerminalSerdeAreDuals t = fromInt (toInt (Tok t)) == Tok t

runPGTest :: IO ()
runPGTest = do
    quickCheckWith (stdArgs { chatty = False }) test_TerminalSerdeAreDuals
    putStrLn "ParserGenTest PASS"