{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main (main) where

import Lumina.Frontend.Lexer (getAllTokensLumina)
import Lumina.Frontend.ParserGen (generateParser, LRParser (LRParser), ppAssocList)
import Lumina.Frontend.LuminaGrammar (luminaGrammar)
import Lumina.Utils (hasDuplicates)
import Lumina.Frontend.Parser (preprocessLumina)
import Lumina.Frontend.Shortcuts (getAST, loadParserFrom)
import Lumina.Interpreter.AstraInterpreter (eval)
import Lumina.Middleend.Shortcuts (optMonaProgram)
import Lumina.Middleend.Astra.HoistFunctions (globaliseFunctions)
import Lumina.Middleend.Mona.Mona (astraToMona)

import qualified Data.Map.Strict as Map
import Control.Monad (forM_)
import Lumina.Interpreter.MonaInterpreter (getMonaValue)
import Lumina.Middleend.Celia.Celia (monaToCelia)

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

demoGlobalisedForm :: IO ()
demoGlobalisedForm = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the globalised form representation. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = globaliseFunctions a
    ppAssocList $ Map.toList env

demoMona :: IO ()
demoMona = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the Mona IR. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = astraToMona a
    forM_ (Map.toList env) (putStrLn . show . snd)

demoOptMona :: IO ()
demoOptMona = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the optimised Mona IR. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = optMonaProgram (astraToMona a)
    forM_ (Map.toList env) (putStrLn . show . snd)

demoMonaInterpreter :: IO ()
demoMonaInterpreter = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll convert it to Mona, optimise it and then interpret it. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let env = optMonaProgram (astraToMona a)
    putStrLn $ show $ getMonaValue env

demoCelia :: IO ()
demoCelia = do
    pars <- loadParserFrom "data/lr1.txt"
    putStrLn "Enter Lumina code and I'll output the Celia IR. Press CTRL-D when you're done."
    inp <- getContents
    let a = fst $ getAST pars inp
    let mtu = optMonaProgram (astraToMona a)
    let ctu = monaToCelia mtu
    forM_ (Map.toList ctu) (putStrLn . show . snd)


main :: IO ()
main = do
    putStrLn "Options:\n\
    \ 1 - Recompile parse tables\n\
    \ 2 - Demo lexer\n\
    \ 3 - Output Astra (Frontend IR)\n\
    \ 4 - Demo Astra interpreter\n\
    \ 5 - Demo hoisted functions\n\
    \ 6 - Demo Mona (Middleend IR - ANF)\n\
    \ 7 - Demo Mona optimiser\n\
    \ 8 - Demo Mona interpreter\n\
    \ 9 - Demo Celia (Middleend IR - C--)"
    i <- readLn :: IO Int
    case i of
        1 -> genAndPrintLR1Parser
        2 -> demoLexer
        3 -> demoParser
        4 -> demoInterpreter
        5 -> demoGlobalisedForm
        6 -> demoMona
        7 -> demoOptMona
        8 -> demoMonaInterpreter
        9 -> demoCelia
        _ -> error $ "Unrecognised option " ++ show i
