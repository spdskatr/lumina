module Lumina.Frontend.Shortcuts (getAST, loadParserFrom) where

import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.Parser (toParserArray, preprocessLumina, producePAST)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Frontend.Lexer (TokenTag, getAllTokensLumina)
import Lumina.Middleend.Astra.Astra (AST, toAST)
import Lumina.Middleend.Typing (LuminaType(..))

-- The power of the entire frontend... in the palm of your hand
getAST :: LRParser LNT TokenTag -> String -> (AST, LuminaType)
getAST lr code = 
    let parseInfo = toParserArray lr
        toks = preprocessLumina $ getAllTokensLumina code
    in toAST $ producePAST toks parseInfo

loadParserFrom :: String -> IO (LRParser LNT TokenTag)
loadParserFrom = fmap read . readFile