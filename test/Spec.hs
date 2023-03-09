import ToyLanguage1 (demoArithGrammar)
import ToyLanguage2 (demoLRValueGrammar)
import ParserGenTest (runPGTest)

main :: IO ()
main = do
    demoArithGrammar
    demoLRValueGrammar
    runPGTest