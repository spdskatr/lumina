{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main (main) where

import Lumina.Frontend.Lexer (getAllTokensLumina)
import Lumina.Frontend.ParserGen (generateParser, LRParser (LRParser), ppAssocList)
import Lumina.Frontend.LuminaGrammar (luminaGrammar)
import Lumina.Utils (hasDuplicates)
import Lumina.Frontend.Parser (preprocessLumina)
import Lumina.Frontend.Shortcuts (getAST, loadParserFrom)
import Lumina.Interpreter.SemanticInterpreter (eval, getValue)
import Lumina.Middleend.Shortcuts (transform, toOptContinuationForm)

import qualified Data.Map.Strict as Map
import Lumina.Middleend.GlobaliseFunctions (globaliseFunctions)

-- WARNING - this may take several minutes to run
genAndPrintLR1Parser :: IO ()
genAndPrintLR1Parser = do
    let (LRParser action goto) = generateParser luminaGrammar
    writeFile "lr1.txt" $ show $ LRParser action goto
    if hasDuplicates action || hasDuplicates goto then
        error "Uh oh, parser generator ran into conflicts. Check your grammar?"
    else
        putStrLn $ "Total number of states: " ++ show (maximum $ map (fst . fst) action);
        putStrLn "Written new parser to lr1.txt. Copy it over to data/lr1.txt when you want to test it."

demoLexer :: IO ()
demoLexer = do
    putStrLn "Enter Lumina code and I'll lex it. Press CTRL-D when you're done."
    interact (show . preprocessLumina . getAllTokensLumina)
    putStrLn ""

demoParser :: IO ()
demoParser = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll parse it. Press CTRL-D when you're done."
    inp <- getContents
    let (a,t) = getAST pars inp
    putStrLn $ show a
    putStrLn $ "Type: " ++ show t

demoInterpreter :: IO ()
demoInterpreter = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll interpret it. Press CTRL-D when you're done."
    inp <- getContents
    let (v,t) = eval pars inp
    putStrLn $ show v ++ " : " ++ show t

demoCPS :: IO ()
demoCPS = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll CPS it. Press CTRL-D when you're done."
    inp <- getContents
    let (a,t) = getAST pars inp
    putStrLn $ show $ transform a
    putStrLn $ "Type: " ++ show t

demoInterpreterCPS :: IO ()
demoInterpreterCPS = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll interpret it. Press CTRL-D when you're done."
    inp <- getContents
    let (a,t) = getAST pars inp
    let v = getValue $ transform a
    putStrLn $ show v ++ " : " ++ show t

demoGlobalisedForm :: IO ()
demoGlobalisedForm = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the globalised form representation. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = globaliseFunctions a
    ppAssocList $ Map.toList env

demoContinuationForm :: IO ()
demoContinuationForm = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the continuation form representation. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = toOptContinuationForm a
    ppAssocList $ Map.toList env


main :: IO ()
main = do
    putStrLn "Options:\n 1 - Recompile parse tables\n\
    \ 2 - Demo lexer\n\
    \ 3 - Demo parser + type checker (Frontend IR)\n\
    \ 4 - Demo interpreter\n\
    \ 5 - Demo CPS\n\
    \ 6 - Demo interpreter with CPS\n\
    \ 7 - Demo globalised form\n\
    \ 8 - Demo continuation form (Middleend IR)"
    i <- readLn :: IO Int
    case i of
        1 -> genAndPrintLR1Parser
        2 -> demoLexer
        3 -> demoParser
        4 -> demoInterpreter
        5 -> demoCPS
        6 -> demoInterpreterCPS
        7 -> demoGlobalisedForm
        8 -> demoContinuationForm
        _ -> error $ "Unrecognised option " ++ show i
