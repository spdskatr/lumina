module ParserGenTest (runPGTest) where

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, quickCheck)
import Lexer (TokenTag)
import ParserGen (Terminal(..), fromInt, toInt)

instance Arbitrary TokenTag where
    arbitrary = arbitraryBoundedEnum

testTerminalSerdeAreDuals :: TokenTag -> Bool
testTerminalSerdeAreDuals t = fromInt (toInt (Tok t)) == (Tok t)

runPGTest :: IO ()
runPGTest = do
    quickCheck testTerminalSerdeAreDuals