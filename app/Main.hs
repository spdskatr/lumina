module Main (main) where

import Lexer (TokenTag, getAllTokensLumina)
import ParserGen (preprocessLumina, generateParser, LRParser)
import LuminaGrammar (luminaGrammar, luminaStart, LNT(..))
--import ParserGen (NonTerminal(..), ParseTable(..), ppAssocList, ppList, closure, getFirst, getNullable, itemsFrom, generateAction)
import Utils (countUp)

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
main = genAndPrintLR1Parser
