{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module ToyLanguage2 (demoLRValueGrammar) where

import Lexer (Tag, Taggable(..))
import ParserGen
import Utils (countUp)
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
    putStrLn "TESTING L/RValue grammar..."
    let first = getFirst toyGrammar (getNullable toyGrammar)
    let myItems = itemsFrom toyGrammar first (closure toyGrammar first [LR1 (LR0 (NonTerminal Start) [] [NTSymb $ NonTerminal Expr]) (EndOfInput)])
    ppAssocList $ countUp myItems
    putStrLn $ "Number of states: " ++ (show $ length myItems)
    ppAssocList $ generateAction toyGrammar first myItems (NonTerminal Start)