module Lumina.Frontend.Shortcuts (getAST) where

import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Frontend.Lexer (TokenTag, getAllTokensLumina)
import Lumina.Frontend.LuminaAST (AST, ASTType, toAST)
import Lumina.Frontend.Parser (toParserArray, preprocessLumina, producePAST)

-- The power of the entire frontend... in the palm of your hand
getAST :: LRParser LNT TokenTag -> String -> (AST, ASTType)
getAST lr code = 
    let parseInfo = toParserArray lr
        toks = preprocessLumina $ getAllTokensLumina code
    in toAST $ producePAST toks parseInfo
