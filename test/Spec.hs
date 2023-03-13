import ToyLanguage1 (demoArithGrammar)
import ToyLanguage2 (demoLRValueGrammar)
import ParserGenTest (runPGTest)
import LuminaGrammarTest (runLGTest)
import LuminaCodeTest (runCodeTest)

main :: IO ()
main = do
    demoArithGrammar
    demoLRValueGrammar
    runPGTest
    runLGTest
    runCodeTest
