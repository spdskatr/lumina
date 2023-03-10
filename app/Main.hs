module Main (main) where

import Lumina.Frontend.Lexer (TokenTag, getAllTokensLumina)
import Lumina.Frontend.ParserGen (generateParser, LRParser (LRParser))
import Lumina.Frontend.LuminaGrammar (luminaGrammar, LNT(..))
import Lumina.Utils (hasDuplicates)
import Lumina.Frontend.Parser (toParserArray, preprocessLumina, producePAST)

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

loadParserFrom :: String -> IO (LRParser LNT TokenTag)
loadParserFrom = fmap read . readFile

demoLexer :: IO ()
demoLexer = do
    putStrLn "Enter Lumina code and I'll lex it. Press CTRL-D when you're done."
    interact (show . preprocessLumina . getAllTokensLumina)
    putStrLn ""

demoParser :: IO ()
demoParser = do
    pars <- loadParserFrom "data/lr1.txt"
    let parseInfo = toParserArray pars
    putStrLn "Enter Lumina code and I'll parse it. Press CTRL-D when you're done."
    inp <- getContents
    let toks = preprocessLumina $ getAllTokensLumina inp
    print $ producePAST toks parseInfo
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Options:\n 1 - Recompile parse tables\n 2 - Demo parser using existing parse table in data/lr1.txt\n 3 - Demo lexer"
    i <- readLn :: IO Int
    case i of
        1 -> genAndPrintLR1Parser
        2 -> demoParser
        3 -> demoLexer
        _ -> error $ "Unrecognised option " ++ show i
