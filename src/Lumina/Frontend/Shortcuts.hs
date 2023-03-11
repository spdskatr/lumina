module Lumina.Frontend.Shortcuts (getAST, eval) where

import Lumina.Frontend.ParserGen (LRParser)
import Lumina.Frontend.LuminaGrammar (LNT)
import Lumina.Frontend.Lexer (TokenTag, getAllTokensLumina)
import Lumina.Frontend.LuminaAST (AST, ASTType, toAST)
import Lumina.Frontend.Parser (toParserArray, preprocessLumina, producePAST)
import Lumina.Interpreter.SemanticInterpreter (Value, getValue)

-- The power of the entire frontend... in the palm of your hand
getAST :: LRParser LNT TokenTag -> String -> (AST, ASTType)
getAST lr code = 
    let parseInfo = toParserArray lr
        toks = preprocessLumina $ getAllTokensLumina code
    in toAST $ producePAST toks parseInfo

eval :: LRParser LNT TokenTag -> String -> (Value, ASTType)
eval lr code = 
    let (ast, astt) = getAST lr code
    in (getValue ast, astt)