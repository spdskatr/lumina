{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module Lumina.Tests.ToyLanguage2 (demoLRValueGrammar) where

import Lumina.Frontend.Lexer (Tag, Taggable(..))
import Lumina.Frontend.ParserGen (generateParser, Production(..), GrammarSymbol(..), NonTerminal(..), Terminal(..))
import Data.Ix (Ix)

data MyTerminal = Star | Assign | Ident String deriving (Show, Eq)
data MyTerminalTag = StarT | AssignT | IdentT deriving (Show, Bounded, Eq, Ord, Enum, Ix)
data MyNonTerminal = Start | Expr | LValue | RValue deriving (Show, Bounded, Eq, Ord, Enum, Ix)

instance Tag MyNonTerminal
instance Tag MyTerminalTag

instance Taggable MyTerminal MyTerminalTag where
    getTag = \case
        Star -> StarT
        Assign -> AssignT
        Ident _ -> IdentT

toyGrammar :: [Production MyNonTerminal MyTerminalTag]
toyGrammar = [
    Production (NonTerminal Start) [NTSymb $ NonTerminal Expr],
    Production (NonTerminal Expr) [NTSymb $ NonTerminal LValue, TSymb $ Tok AssignT, NTSymb $ NonTerminal RValue],
    Production (NonTerminal Expr) [NTSymb $ NonTerminal RValue],
    Production (NonTerminal LValue) [TSymb $ Tok StarT, NTSymb $ NonTerminal RValue],
    Production (NonTerminal LValue) [TSymb $ Tok IdentT],
    Production (NonTerminal RValue) [NTSymb $ NonTerminal LValue]]

demoLRValueGrammar :: IO ()
demoLRValueGrammar = do
    putStrLn $ show $ generateParser toyGrammar