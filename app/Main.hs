module Main (main) where

import Lumina.Frontend.Lexer (TokenTag, getAllTokensLumina)
import Lumina.Frontend.ParserGen (preprocessLumina, generateParser, LRParser)
import Lumina.Frontend.LuminaGrammar (luminaGrammar, LNT(..))
import Lumina.Utils (countUp)

-- WARNING - this may take several minutes to run
genAndPrintLR1Parser :: IO ()
genAndPrintLR1Parser = do
    print $ generateParser luminaGrammar

loadParserFrom :: String -> IO (LRParser LNT TokenTag)
loadParserFrom = fmap read . readFile

demoLexer :: IO ()
demoLexer = do
    putStrLn "Enter Lumina code and I'll lex it. Press CTRL-D when you're done."
    interact (show . preprocessLumina . getAllTokensLumina)
    putStrLn ""

main :: IO ()
main = demoLexer
