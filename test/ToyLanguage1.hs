{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module ToyLanguage1 (demoArithGrammar) where

import Lumina.Frontend.Lexer (Tag, Taggable(..))
import Lumina.Frontend.ParserGen (generateParser, Production(..), GrammarSymbol(..), NonTerminal(..), Terminal(..))
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
    putStrLn $ show $ generateParser toyGrammar