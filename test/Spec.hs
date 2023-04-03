import Lumina.Tests.ToyLanguage1 (demoArithGrammar)
import Lumina.Tests.ToyLanguage2 (demoLRValueGrammar)
import Lumina.Tests.ParserGenTest (runPGTest)
import Lumina.Tests.LuminaGrammarTest (runLGTest)
import Lumina.Tests.LuminaCodeTest (runCodeTest)

main :: IO ()
main = do
    demoArithGrammar
    demoLRValueGrammar
    runPGTest
    runLGTest
    runCodeTest
