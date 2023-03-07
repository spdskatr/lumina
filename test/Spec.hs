{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

import Lexer (Tag(..), Taggable(..))
import Parser
import Data.Ix (Ix)

import Control.Monad (forM_)

data ToyTerminal = Value Int | Add | Mul | LParen | RParen deriving (Eq, Show)
data ToyTerminalTag = ValueT | AddT | MulT | LParenT | RParenT deriving (Bounded, Eq, Enum, Ord, Ix, Show)

instance Tag ToyTerminalTag
instance Taggable ToyTerminal ToyTerminalTag where
    getTag = \case
        Value _ -> ValueT
        Add     -> AddT
        Mul     -> MulT
        LParen  -> LParenT
        RParen  -> RParenT

data ToyNonTerminal = Start | Expr | Term | Factor deriving (Bounded, Eq, Enum, Ord, Ix, Show)

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

toyGrammarLL :: [Production ToyNonTerminal ToyTerminalTag]
toyGrammarLL = [
    Production (NonTerminal Start) [NTSymb $ NonTerminal Expr],
    Production (NonTerminal Expr) [TSymb $ Tok AddT, NTSymb $ NonTerminal Expr, NTSymb $ NonTerminal Term],
    Production (NonTerminal Expr) [NTSymb $ NonTerminal Term],
    Production (NonTerminal Term) [TSymb $ Tok MulT, NTSymb $ NonTerminal Term, NTSymb $ NonTerminal Factor],
    Production (NonTerminal Term) [NTSymb $ NonTerminal Factor],
    Production (NonTerminal Factor) [TSymb $ Tok LParenT, NTSymb $ NonTerminal Expr, TSymb $ Tok RParenT],
    Production (NonTerminal Factor) [TSymb $ Tok ValueT]]

ppAssocList :: (Show a, Show b) => [(a, b)] -> IO ()
ppAssocList x = let y = map (\(a, b) -> show a ++ ": \t" ++ show b) x
    in forM_ y putStrLn

ppParseTable :: (Show n, Show tt, Show b) => ParseTable n tt b -> IO ()
ppParseTable (ParseTable x) = ppAssocList x

ppList :: (Show a) => [a] -> IO ()
ppList x = forM_ (map show x) putStrLn

main :: IO ()
main = do
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