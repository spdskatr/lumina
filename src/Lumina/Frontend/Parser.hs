{-# LANGUAGE TupleSections #-}
module Lumina.Frontend.Parser (
    LRParserArray(..),
    ParserState(..),
    toParserArray,
    preprocessLumina,
    parseLumina,
    producePAST
) where

import Lumina.Frontend.ParserGen (ParseAction (..), Terminal (..), LRParser (..), Production (..), NonTerminal (..))
import Lumina.Frontend.Lexer (TokenTag, Token (..), getTag)
import Lumina.Frontend.LuminaGrammar (LNT, PAST (..), luminaReduceActions, luminaGrammar)
import Lumina.Utils (headMaybe, orElse, internalError, parseError)

import Control.Monad.Trans.State.Strict (State, get, modify, evalState)
import Data.Array (Array, array, listArray, (!))
import Data.List (sort)
import Data.Ix (Ix(..))

type Term = Terminal TokenTag
type Prod = Production LNT TokenTag

-- Preprocessing - Filter out all comments and whitespace
preprocessLumina :: [Token] -> [Token]
preprocessLumina []     = []
preprocessLumina (x:xs) = case x of
    Whitespace -> preprocessLumina xs
    Comment    -> preprocessLumina xs
    _          -> x : preprocessLumina xs

-- Action, Goto, Reduce stack transformations
data LRParserArray = LRParserArray {
    getAction :: Array (Int, Term) (Maybe ParseAction),
    getGoto :: Array (Int, LNT) (Maybe Int),
    getGrammar :: Array Int Prod,
    getReduction :: Array Int ([(Int, PAST)] -> PAST)
}

data ParserState = ParserState [(Int, PAST)] [Token] deriving (Show, Eq)

(<~>) :: LRParserArray -> (Int, Term) -> ParseAction
lr <~> i = case getAction lr ! i of
    Nothing -> parseError $ "Tried to ACTION " ++ show i ++ " but it doesn't exist."
    Just k -> k

lookupGoto :: LRParserArray -> (Int, LNT) -> Int
lookupGoto lr i = case getGoto lr ! i of
    Nothing -> parseError $ "Tried to GOTO " ++ show i ++ " but it doesn't exist."
    Just k -> k

(?) :: LRParserArray -> Int -> Prod
lr ? i = getGrammar lr ! i

makeNodeByReduction :: LRParserArray -> Int -> [(Int, PAST)] -> PAST
makeNodeByReduction lr i = getReduction lr ! i

fillInParallel :: (Ord a) => [a] -> [(a,b)] -> [(a,Maybe b)]
fillInParallel [] xs = map (fmap Just) xs
fillInParallel xs [] = map (, Nothing) xs
fillInParallel (x:xs) (y:ys) =
    if x < fst y then
        (x, Nothing) : fillInParallel xs (y:ys)
    else
        (fst y, Just $ snd y) : fillInParallel xs ys

-- Convert data to array for fast access
fillBlanks :: (Ix a, Ord b) => a -> a -> [(a,b)] -> [(a,Maybe b)]
fillBlanks lo up res = fillInParallel (range (lo,up)) (sort res)

toArray :: (Ix a, Ix b, Ord c) => [((a,b), c)] -> Array (a,b) (Maybe c)
toArray a = array ((mina, minb), (maxa, maxb)) $ fillBlanks (mina, minb) (maxa, maxb) a
    where
        mina = minimum $ map (fst . fst) a
        maxa = maximum $ map (fst . fst) a
        minb = minimum $ map (snd . fst) a
        maxb = maximum $ map (snd . fst) a

toParserArray :: LRParser LNT TokenTag -> LRParserArray
toParserArray (LRParser action goto') =
    LRParserArray {
        getAction = toArray action,
        getGoto = toArray goto',
        getGrammar = listArray (0, length luminaGrammar - 1) luminaGrammar,
        getReduction = listArray (0, length luminaGrammar - 1) luminaReduceActions
    }

-- Reduce action

reduce :: LRParserArray -> Int -> ParserState -> ParserState
reduce lr i (ParserState s s2) = ParserState news s2
    where
        (Production (NonTerminal nt) symbs) = lr ? i
        (hd, tl) = splitAt (length symbs) s
        newSymb = makeNodeByReduction lr i (reverse hd)
        prevState = (fst <$> headMaybe tl) `orElse` 0
        news = (lookupGoto lr (prevState, nt), newSymb) : tl

shift :: LRParserArray -> Int -> ParserState -> ParserState
shift _ newstate (ParserState s s2) = case s2 of
    [] -> internalError "Parser wanted to shift, but no more symbols to shift."
    (tok:rest) -> ParserState ((newstate, PToken tok) : s) rest

initState :: [Token] -> ParserState
initState = ParserState []

parseLumina :: LRParserArray -> State ParserState PAST
parseLumina lr = do
    (ParserState hd tl) <- get
    let nextTok = (Tok . getTag <$> headMaybe tl) `orElse` EndOfInput
    let (topState, topSymb) = headMaybe hd `orElse` (0, internalError "Tried to accept top of stack but it does not exist.")

    case lr <~> (topState, nextTok) of
        Shift n -> modify (shift lr n) >> parseLumina lr
        Reduce n -> modify (reduce lr n) >> parseLumina lr
        Accept -> return topSymb

producePAST :: [Token] -> LRParserArray -> PAST
producePAST toks lr = evalState (parseLumina lr) (initState toks)