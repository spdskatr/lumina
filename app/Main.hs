module Main (main) where

import Lexer
import ParserGen
import LuminaGrammar (luminaGrammar, luminaStart, LNT(..))
--import ParserGen (NonTerminal(..), ParseTable(..), ppAssocList, ppList, closure, getFirst, getNullable, itemsFrom, generateAction)
import Utils (countUp)

-- WARNING - this may take several minutes to run
genAndPrintLR1ParseTable :: IO ()
genAndPrintLR1ParseTable = do
    let first = getFirst luminaGrammar (getNullable luminaGrammar)
    let myItems = itemsFrom luminaGrammar first (closure luminaGrammar first [luminaStart])
    print $ generateAction luminaGrammar first myItems (NonTerminal Start)

demoLexer :: IO ()
demoLexer = do
    putStrLn "Enter Lumina code and I'll lex it. Press CTRL-D when you're done."
    interact (show . preprocessLumina . getAllTokensLumina)
    putStrLn ""

main :: IO ()
main = genAndPrintLR1ParseTable
