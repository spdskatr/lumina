{-# LANGUAGE LambdaCase #-}

{-
 - Parser library generates LALR(1) parse table and then executes the parse.
 - 
 - It generates the table the inefficient way - generate all LR(1) items and
 - then merge by core, then generate the action and goto tables LR(1) style
 -}

module ParserGen (
    NonTerminal(..),
    Terminal(..),
    GrammarSymbol(..),
    Production(..),
    ParseTable(..),
    LR0Item(..),
    LR1Item(..),
    ParseAction(..),
    LRParser(..),
    (!),
    ppAssocList,
    ppParseTable,
    ppList,
    preprocessLumina,
    getNullable,
    getFirst,
    itemsFrom,
    closure,
    generateAction,
    generateParser
) where

import Control.Monad (guard, forM_)
import Data.List (findIndex, nub, elemIndex)
import Data.Function (on)
import Data.List.Extra (enumerate)
import Data.Ix (Ix(..))

import Lexer (Token (..), Tag)
import Utils (orElse, update, untilFixedPoint, countUp)

data NonTerminal n = NonTerminal n deriving (Eq, Show)
data Terminal tt = Tok tt | Epsilon | EndOfInput deriving (Eq, Show, Read)
data GrammarSymbol n tt = TSymb (Terminal tt) | NTSymb (NonTerminal n) deriving (Eq, Show)
data Production n tt = Production (NonTerminal n) [GrammarSymbol n tt] deriving (Eq, Show)
newtype ParseTable n tt a = ParseTable [(GrammarSymbol n tt, a)] deriving (Eq, Show)
data LR0Item n tt = LR0 (NonTerminal n) [GrammarSymbol n tt] [GrammarSymbol n tt] deriving (Eq, Show)
data LR1Item n tt = LR1 (LR0Item n tt) (Terminal tt) deriving (Eq, Show)
data ParseAction = Shift Int | Reduce Int | Accept deriving (Eq, Show, Read)
data LRParser n tt = LRParser [((Int, Terminal tt), ParseAction)] [((Int, n), Int)] deriving (Eq, Read, Show)

-- Utils for indexing the action table
toInt :: (Tag tt) => Terminal tt -> Int
toInt Epsilon = 0
toInt EndOfInput = 1
toInt (Tok tag) = 2 + fromEnum tag

fromInt :: (Tag tt) => Int -> Terminal tt
fromInt 0 = Epsilon
fromInt 1 = EndOfInput
fromInt x = Tok $ toEnum (x-2)

instance (Tag tt) => Ord (Terminal tt) where
    compare = compare `on` toInt

instance (Tag tt) => Ix (Terminal tt) where
    range (l, r) = map fromInt $ range (toInt l, toInt r)
    index (l, r) a = index (toInt l, toInt r) (toInt a)
    inRange (l, r) a = inRange (toInt l, toInt r) (toInt a)

-- Utils for Parse tables
(!) :: (Eq tt, Eq n) => ParseTable n tt a -> GrammarSymbol n tt -> Maybe a
(ParseTable t) ! s = lookup s t

-- Pretty-printing

ppAssocList :: (Show a, Show b) => [(a, b)] -> IO ()
ppAssocList x = let y = map (\(a, b) -> show a ++ ": \t" ++ show b) x
    in forM_ y putStrLn

ppParseTable :: (Show n, Show tt, Show b) => ParseTable n tt b -> IO ()
ppParseTable (ParseTable x) = ppAssocList x

ppList :: (Show a) => [a] -> IO ()
ppList x = forM_ (map show x) putStrLn

-- Preprocessing - Filter out all comments and whitespace, add End of Input
preprocessLumina :: [Token] -> [Terminal Token]
preprocessLumina []     = [EndOfInput]
preprocessLumina (x:xs) = case x of
    Whitespace -> preprocessLumina xs
    Comment    -> preprocessLumina xs
    _          -> Tok x : preprocessLumina xs

allTags :: (Tag tt) => [tt]
allTags = enumerate

-- Does not include epsilon or EndOfInput symbol
allGrammarSymbols :: (Tag n, Tag tt) => [GrammarSymbol n tt]
allGrammarSymbols = map (TSymb . Tok) allTags ++ map (NTSymb . NonTerminal) allTags

-- NULLABLE
initNullable :: [(GrammarSymbol n tt, Bool)]
initNullable = [(TSymb Epsilon, True)]

iterNullable :: (Tag tt, Eq n) => [Production n tt] -> [(GrammarSymbol n tt, Bool)] -> [(GrammarSymbol n tt, Bool)]
iterNullable p nullableList = foldl (update (||)) nullableList $ do
    (Production n tl) <- p
    let check = all (\symb -> lookup symb nullableList `orElse` False) tl
    guard check
    return $ (NTSymb n, check)

getNullable :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt Bool
getNullable p = ParseTable $ untilFixedPoint (iterNullable p) initNullable

-- FIRST
initFirst :: (Tag tt) => [(GrammarSymbol n tt, [Terminal tt])]
initFirst = (TSymb Epsilon, [Epsilon]) : (TSymb EndOfInput, [EndOfInput]) : map (\t -> (TSymb (Tok t), [Tok t])) allTags

iterFirst :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt Bool -> [(GrammarSymbol n tt, [Terminal tt])] -> [(GrammarSymbol n tt, [Terminal tt])]
iterFirst p nullable firstList = foldl (update (\a b -> nub $ a ++ b)) firstList $ do
    (Production n tl) <- p
    let i = findIndex (\a -> not $ nullable ! a `orElse` False) tl `orElse` length tl
    let res = filter (/= Epsilon) $ concat $ map (\s -> lookup s firstList `orElse` []) $ take (i+1) tl
    if i == length tl then
        return (NTSymb n, Epsilon : res)
    else
        return (NTSymb n, res)

getFirst :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt Bool -> ParseTable n tt [Terminal tt]
getFirst p nullable = ParseTable $ untilFixedPoint (iterFirst p nullable) initFirst

lookupFirst :: (Tag tt, Tag n) => ParseTable n tt [Terminal tt] -> [GrammarSymbol n tt] -> [Terminal tt]
lookupFirst _     []     = [Epsilon]
lookupFirst first (x:xs) = 
    let someResults = (first ! x `orElse` []) in 
        if Epsilon `elem` someResults then 
            nub $ filter (/= Epsilon) someResults ++ lookupFirst first xs
        else
            nub $ filter (/= Epsilon) someResults

-- LR(1) CLOSURE
productionsFor :: (Tag n) => [Production n tt] -> NonTerminal n -> [Production n tt]
productionsFor p a = filter (\(Production b _) -> a == b) p

getBeginItems :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> NonTerminal n -> [GrammarSymbol n tt] -> [LR1Item n tt]
getBeginItems p first a prec = do
    (Production _ tl) <- productionsFor p a
    b <- lookupFirst first prec
    return $ LR1 (LR0 a [] tl) b

stepClosure :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> [LR1Item n tt]
stepClosure p first items = nub $ items ++ do
    LR1 (LR0 _ _ nx) a <- items
    case nx of
        (NTSymb (NonTerminal n2):xs) -> getBeginItems p first (NonTerminal n2) (xs ++ [TSymb a])
        _ -> []

closure :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> [LR1Item n tt]
closure p first = untilFixedPoint (stepClosure p first)

-- GOTO
goto :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> GrammarSymbol n tt -> [LR1Item n tt]
goto p first items e = closure p first $ do
    (LR1 (LR0 n hd tl) la) <- items
    case tl of
        (x:xs) -> if x == e then [LR1 (LR0 n (x:hd) xs) la] else []
        _ -> []

-- LR(1) items
stepItems :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> ([[LR1Item n tt]], [[LR1Item n tt]]) -> ([[LR1Item n tt]], [[LR1Item n tt]])
stepItems p first (orig, elems) = (old, new) 
    where
        old = nub $ orig ++ elems
        new = do
            e <- elems
            x <- allGrammarSymbols
            let nx = goto p first e x
            guard $ not (null nx)
            guard $ not (nx `elem` old)
            return nx

itemsFrom :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [LR1Item n tt] -> [[LR1Item n tt]]
itemsFrom p first e = fst $ untilFixedPoint (stepItems p first) ([], [e])

-- LR(1) parse table generator
-- Code assumes that the first state in the list of items is the start state.
generateAction :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [[LR1Item n tt]] -> NonTerminal n -> [((Int, Terminal tt), ParseAction)]
generateAction p first states start = nub $ do
    (ind, state) <- countUp states
    (LR1 (LR0 n hd tl) la) <- state
    case tl of
        TSymb (Tok tag) : _ -> do
            let nx = goto p first state (TSymb (Tok tag))
            case elemIndex nx states of
                Just i -> return ((ind, Tok tag), Shift i)
                Nothing -> if null nx then [] else error $ "Internal parser error, state " ++ show nx ++ " not found"
        [] -> do
            if n /= start then
                return ((ind, la), Reduce $ elemIndex (Production n (reverse hd ++ tl)) p `orElse`
                    (error $ "Internal parser error, production " ++ show n ++ " -> " ++ show (reverse hd ++ tl) ++ " not found"))
            else
                return ((ind, EndOfInput), Accept)
        _ -> []

generateGoto :: (Tag tt, Tag n) => [Production n tt] -> ParseTable n tt [Terminal tt] -> [[LR1Item n tt]] -> [((Int, n), Int)]
generateGoto p first states = nub $ do
    (ind, state) <- countUp states
    a <- allTags
    let res = goto p first state (NTSymb $ NonTerminal a)
    case elemIndex res states of
        Just i -> return ((ind, a), i)
        Nothing -> if null res then [] else error $ "Internal parser error, state " ++ show res ++ " not found"

-- generateParser assumes the starting production is the first one in the list.
generateParser :: (Tag tt, Tag n) => [Production n tt] -> LRParser n tt
generateParser p = 
        LRParser (generateAction p first myItems (NonTerminal start)) (generateGoto p first myItems)
    where
        (Production (NonTerminal start) tl) = head p
        first = getFirst p (getNullable p)
        myItems = itemsFrom p first (closure p first [LR1 (LR0 (NonTerminal start) [] tl) EndOfInput])
    
-- TODO: LALR(1) some day