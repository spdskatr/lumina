import ToyLanguage1 (demoArithGrammar)
import ToyLanguage2 (demoLRValueGrammar)
import ParserGenTest (runPGTest)
import LuminaGrammarTest (runLGTest)

main :: IO ()
main = do
    demoArithGrammar
    demoLRValueGrammar
    runPGTest
    runLGTest