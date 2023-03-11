{-# OPTIONS_GHC -Wno-orphans #-}
module ParserGenTest (runPGTest) where

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, quickCheck)
import Lumina.Frontend.Lexer (TokenTag)
import Lumina.Frontend.ParserGen (Terminal(..), fromInt, toInt)

instance Arbitrary TokenTag where
    arbitrary = arbitraryBoundedEnum

test_TerminalSerdeAreDuals :: TokenTag -> Bool
test_TerminalSerdeAreDuals t = fromInt (toInt (Tok t)) == (Tok t)

runPGTest :: IO ()
runPGTest = do
    quickCheck test_TerminalSerdeAreDuals