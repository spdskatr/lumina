{-# LANGUAGE LambdaCase #-}

{-
 - Parser library generates LALR(1) parse table and then executes the parse.
 - 
 - It generates the table the inefficient way - generate all LR(1) items and
 - then merge by core, then generate the action and goto tables LR(1) style
 -}

module Parser where

import Control.Monad (guard)
import Data.List (findIndex, nub)
import Data.List.Extra (enumerate)

import Lexer (Token (..), Tag)
import Utils (orElse, update, untilFixedPoint)

data NonTerminal n = NonTerminal n deriving (Eq, Show)
data Terminal tt = Tok tt | Epsilon | EndOfInput deriving (Eq, Show)
data GrammarSymbol n tt = TSymb (Terminal tt) | NTSymb (NonTerminal n) deriving (Eq, Show)
data Production n tt = Production (NonTerminal n) [GrammarSymbol n tt] deriving (Eq, Show)
newtype ParseTable n tt a = ParseTable [(GrammarSymbol n tt, a)] deriving (Eq, Show)
data LR0Item n tt = LR0 (NonTerminal n) [GrammarSymbol n tt] [GrammarSymbol n tt] deriving (Eq, Show)
data LR1Item n tt = LR1 (LR0Item n tt) (Terminal tt) deriving (Eq, Show)

-- Utils for Parse tables
(!) :: (Eq tt, Eq n, Eq a) => ParseTable n tt a -> GrammarSymbol n tt -> Maybe a
(ParseTable t) ! s = lookup s t

-- Preprocessing - Filter out all comments and whitespace, add End of Input
preprocessLumina :: [Token] -> [Terminal Token]
preprocessLumina []     = [EndOfInput]
preprocessLumina (x:xs) = case x of
    Whitespace -> preprocessLumina xs
    Comment    -> preprocessLumina xs
    _          -> Tok x : preprocessLumina xs

allTags :: (Tag tt) => [tt]
allTags = enumerate

-- NULLABLE
initNullable :: (Tag tt) => [(GrammarSymbol n tt, Bool)]
initNullable = [(TSymb Epsilon, True)]

iterNullable :: (Tag tt, Eq n) => [Production n tt] -> [(GrammarSymbol n tt, Bool)] -> [(GrammarSymbol n tt, Bool)]
iterNullable p nullableList = foldl (update (||)) nullableList $ do
    (Production n tl) <- p
    let check = all (\symb -> lookup symb nullableList `orElse` False) tl
    guard check
    return $ (NTSymb n, check)

getNullable :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt Bool
getNullable p = ParseTable $ untilFixedPoint (iterNullable p) initNullable

-- FIRST
initFirst :: (Tag tt) => [(GrammarSymbol n tt, [Terminal tt])]
initFirst = (TSymb Epsilon, [Epsilon]) : (TSymb EndOfInput, [EndOfInput]) : map (\t -> (TSymb (Tok t), [Tok t])) allTags

iterFirst :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt Bool -> [(GrammarSymbol n tt, [Terminal tt])] -> [(GrammarSymbol n tt, [Terminal tt])]
iterFirst p nullable firstList = foldl (update (\a b -> nub $ a ++ b)) firstList $ do
    (Production n tl) <- p
    let i = findIndex (\a -> not $ nullable ! a `orElse` False) tl `orElse` length tl
    let res = filter (/= Epsilon) $ concat $ map (\s -> lookup s firstList `orElse` []) $ take (i+1) tl
    if i == length tl then
        return (NTSymb n, Epsilon : res)
    else
        return (NTSymb n, res)

getFirst :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt Bool -> ParseTable n tt [Terminal tt]
getFirst p nullable = ParseTable $ untilFixedPoint (iterFirst p nullable) initFirst

lookupFirst :: (Tag tt, Eq n) => ParseTable n tt [Terminal tt] -> [GrammarSymbol n tt] -> [Terminal tt]
lookupFirst first []     = [Epsilon]
lookupFirst first (x:xs) = 
    let someResults = (first ! x `orElse` []) in 
        if Epsilon `elem` someResults then 
            nub $ filter (/= Epsilon) someResults ++ lookupFirst first xs
        else
            nub $ filter (/= Epsilon) someResults

-- LR(1) CLOSURE
productionsFor :: (Eq n) => [Production n tt] -> NonTerminal n -> [Production n tt]
productionsFor p a = filter (\(Production b _) -> a == b) p

getBeginItems :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> NonTerminal n -> [GrammarSymbol n tt] -> [LR1Item n tt]
getBeginItems p first a prec = do
    (Production _ tl) <- productionsFor p a
    b <- lookupFirst first prec
    return $ LR1 (LR0 a [] tl) b

stepClosure :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> [LR1Item n tt]
stepClosure p first items = nub $ items ++ do
    LR1 (LR0 n1 _ nx) a <- items
    case nx of
        (NTSymb (NonTerminal n2):xs) -> getBeginItems p first (NonTerminal n2) (xs ++ [TSymb a])
        _ -> []

closure :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> [LR1Item n tt]
closure p first = untilFixedPoint (stepClosure p first)

-- GOTO
goto :: (Tag tt, Eq n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> GrammarSymbol n tt -> [LR1Item n tt]
goto p first items elem = closure p first $ do
    (LR1 (LR0 n hd tl) la) <- items
    case tl of
        (x:xs) -> if x == elem then [LR1 (LR0 n (x:hd) xs) la] else []
        _ -> []

-- LR(1) parse table generator