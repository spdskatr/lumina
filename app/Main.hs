module Main (main) where

import Lexer
import ParserGen

main :: IO ()
main = do
    putStrLn "Enter Lumina code and I'll lex it. Press CTRL-D when you're done."
    interact (show . preprocessLumina . getAllTokensLumina)
    putStrLn ""
