module LuminaGrammarTest (runLGTest) where

import Lumina.Frontend.Lexer (Token(..), TokenTag)
import Lumina.Frontend.ParserGen (Production(..), NonTerminal(..))
import Lumina.Frontend.LuminaGrammar (luminaAnnotatedGrammar, PAST(..), LNT(..))

import Control.Monad (forM_)

test_GrammarCanAcceptRightNumberOfSymbols :: ([(Int, PAST)] -> PAST, Production LNT TokenTag) -> Either String ()
test_GrammarCanAcceptRightNumberOfSymbols (f,p) =
    let (Production n tl) = p
    in do
        if n == NonTerminal Start || f (replicate (length tl) (0, PCaseList [])) /= PToken Comment then
            return ()
        else
            Left $ "test_GrammarCanAcceptRightNumberOfSymbols FAIL on " ++ show p

runLGTest :: IO ()
runLGTest = do
    case forM_ luminaAnnotatedGrammar test_GrammarCanAcceptRightNumberOfSymbols of
        Left err -> error err
        Right _ -> putStrLn "LuminaGrammarTest PASS"