{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module ToyLanguage1 (demoArithGrammar) where

import Lexer (Tag, Taggable(..))
import ParserGen
import Data.Ix (Ix)

data ToyTerminal = Value Int | Add | Mul | LParen | RParen deriving (Eq, Show)
data ToyTerminalTag = ValueT | AddT | MulT | LParenT | RParenT deriving (Bounded, Eq, Enum, Ord, Ix, Show)
data ToyNonTerminal = Start | Expr | Term | Factor deriving (Bounded, Eq, Enum, Ord, Ix, Show)

instance Tag ToyTerminalTag
instance Tag ToyNonTerminal
instance Taggable ToyTerminal ToyTerminalTag where
    getTag = \case
        Value _ -> ValueT
        Add     -> AddT
        Mul     -> MulT
        LParen  -> LParenT
        RParen  -> RParenT


toyGrammar :: [Production ToyNonTerminal ToyTerminalTag]
toyGrammar = [
    Production (NonTerminal Start) [NTSymb $ NonTerminal Expr],
    -- Production (NonTerminal Term) [TSymb Epsilon],
    Production (NonTerminal Expr) [NTSymb $ NonTerminal Expr, TSymb $ Tok AddT, NTSymb $ NonTerminal Term],
    Production (NonTerminal Expr) [NTSymb $ NonTerminal Term],
    Production (NonTerminal Term) [NTSymb $ NonTerminal Term, TSymb $ Tok MulT, NTSymb $ NonTerminal Factor],
    Production (NonTerminal Term) [NTSymb $ NonTerminal Factor],
    Production (NonTerminal Factor) [TSymb $ Tok LParenT, NTSymb $ NonTerminal Expr, TSymb $ Tok RParenT],
    Production (NonTerminal Factor) [TSymb $ Tok ValueT]]


demoArithGrammar :: IO ()
demoArithGrammar = do
    putStrLn "Generating..."
    putStrLn $ show $ (initNullable :: [(GrammarSymbol ToyNonTerminal ToyTerminalTag, Bool)])
    putStrLn $ show $ iterNullable toyGrammar initNullable
    let nullable = getNullable toyGrammar
    ppParseTable nullable
    putStrLn "----- TEST 2"
    putStrLn $ show $ (initFirst :: [(GrammarSymbol ToyNonTerminal ToyTerminalTag, [Terminal ToyTerminalTag])])
    putStrLn $ show $ iterFirst toyGrammar nullable initFirst
    let first = getFirst toyGrammar nullable
    ppParseTable first
    putStrLn $ show $ lookupFirst first [NTSymb $ NonTerminal Term, NTSymb $ NonTerminal Expr, TSymb $ Tok RParenT]
    putStrLn "----- TEST 3"
    let clos = closure toyGrammar first [LR1 (LR0 (NonTerminal Start) [] [NTSymb $ NonTerminal Expr]) (EndOfInput)]
    ppList clos
    putStrLn $ "----- TEST 4"
    ppList $ goto toyGrammar first clos (NTSymb $ NonTerminal Expr)
    putStrLn "----- TEST 5"
    let myItems = itemsFrom toyGrammar first clos
    ppList myItems
    putStrLn $ "Number of states: " ++ (show $ length myItems)