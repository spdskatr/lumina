{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

import ToyLanguage1 (demoArithGrammar)
import ToyLanguage2 (demoLRValueGrammar)

main :: IO ()
main = do
    demoArithGrammar
    demoLRValueGrammar