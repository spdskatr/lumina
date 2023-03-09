{-# LANGUAGE LambdaCase, ExistentialQuantification, MultiParamTypeClasses #-}

module Lexer (
    Token (..),
    Tag,
    Taggable (..),
    TokenTag (..),
    StateMachine,
    AnyStateMachine,
    testStateMachine,
    getAllTokens,
    getAllTokensLumina
) where

{-
 - Cursed manual Lexer implementation. I match a list of "DFAs" to the current
 - string at each step to determine what to do next. I guess you could call it
 - an "NFA" if you really wanted to stretch it haha
 - 
 - The DFAs are State monads.
 - 
 - I think the only thing more cursed would be my LALR(1) parser generator.
 -}

import Control.Monad (forM)
import Control.Monad.Trans.State.Strict (State, state, runState, evalState)
import Data.Char (isAlpha, isNumber, ord)
import Data.Ix (Ix)
import Data.Maybe (mapMaybe)

class (Bounded t, Enum t, Ord t, Ix t, Show t) => Tag t

class (Tag t) => Taggable x t where
    getTag :: x -> t

-- Keep 0 and 1 tokens separate because they can take on other types
data Token
    = Zero
    | One
    | IntLit Int
    | LParen
    | RParen
    | Colon
    | Semicolon
    | Bang
    | Assign
    | LessThan
    | Ref
    | IntType
    | BoolType
    | UnitType
    | Add
    | Sub
    | Mul
    | AndOp
    | OrOp
    | Bar
    | Arrow
    | Not
    | Equal
    | Fun
    | With
    | Do
    | End
    | While
    | Case
    | Ident String
    | Whitespace
    | Comment
    deriving (Eq, Show)

-- For the benefit of the parser.
data TokenTag
    = ZeroT
    | OneT
    | IntLitT
    | LParenT
    | RParenT
    | ColonT
    | SemicolonT
    | BangT
    | AssignT
    | LessThanT
    | RefT
    | IntTypeT
    | BoolTypeT
    | UnitTypeT
    | AddT
    | SubT
    | MulT
    | AndOpT
    | OrOpT
    | BarT
    | ArrowT
    | NotT
    | EqualT
    | FunT
    | WithT
    | DoT
    | EndT
    | WhileT
    | CaseT
    | IdentT
    | WhitespaceT
    | CommentT     
    deriving (Bounded, Eq, Show, Enum, Ord, Ix)

instance Tag TokenTag

instance Taggable Token TokenTag where
    getTag = \case
        Zero       -> ZeroT
        One        -> OneT
        IntLit _   -> IntLitT
        LParen     -> LParenT
        RParen     -> RParenT
        Colon      -> ColonT
        Semicolon  -> SemicolonT
        Bang       -> BangT
        Assign     -> AssignT
        LessThan   -> LessThanT
        Ref        -> RefT
        IntType    -> IntTypeT
        BoolType   -> BoolTypeT
        UnitType   -> UnitTypeT
        Add        -> AddT
        Sub        -> SubT
        Mul        -> MulT
        AndOp      -> AndOpT
        OrOp       -> OrOpT
        Bar        -> BarT
        Arrow      -> ArrowT
        Not        -> NotT
        Equal      -> EqualT
        Fun        -> FunT
        With       -> WithT
        Do         -> DoT
        End        -> EndT
        While      -> WhileT
        Case       -> CaseT
        Ident _    -> IdentT
        Whitespace -> WhitespaceT
        Comment    -> CommentT

data MatchState = Wait | Accept Token | Reject deriving (Eq, Show)

-- Matchers
--
-- I've sort of kept them as a state machine so I can technically say they're
-- constant time but yea

-- State machine with internal state type a
data StateMachine a = StateMachine a (Char -> State a MatchState)

-- Existential state machine type to implement a common stepping function
data AnyStateMachine = forall a. USM (StateMachine a)

testStateMachine :: StateMachine a -> String -> [MatchState]
testStateMachine (StateMachine s cont) inp = evalState stateIter s
    where stateIter = forM inp cont

matchString :: Token -> String -> StateMachine String
matchString tok m = StateMachine m $ \c -> state $ \case
    []     -> (Reject, [])
    (x:xs) -> if x == c then (if xs == [] then (Accept tok, xs) else (Wait, xs)) else (Reject, [])

-- TODO: Deal with bounds checking
matchAnyPositiveInt :: StateMachine Int
matchAnyPositiveInt = StateMachine 0 $ \c -> state $ \s ->
    if '1' <= c && c <= '9' && s >= 0 then (Accept (IntLit $ s * 10 + getDigit c), s * 10 + getDigit c)
    else if c == '0'        && s >= 1 then (Accept (IntLit $ s * 10), s * 10)
    else (Reject, -1)
    where
        getDigit c = ord c - ord '0'

-- TODO: Allow ident to have arbitrary unicode characters
matchAnyIdent :: StateMachine (Maybe String)
matchAnyIdent = StateMachine (Just "") $ \c -> state $ \case
    Nothing    -> (Reject, Nothing)
    Just ident -> let nextIdent = ident ++ [c] in case ident of
        "" -> if isOkFirstCharInIdent c then (Accept (Ident nextIdent), Just nextIdent) else (Reject, Nothing)
        _  -> if isOkInIdent c          then (Accept (Ident nextIdent), Just nextIdent) else (Reject, Nothing)
    where
        isOkFirstCharInIdent c = isAlpha c || c == '_'
        isOkInIdent c = isOkFirstCharInIdent c || isNumber c || c == '\''

matchComment :: StateMachine Int
matchComment = StateMachine 0 $ \c -> state $ \case
    0 -> if c == '(' then (Wait, 1) else (Reject, 5)
    1 -> if c == '*' then (Wait, 2) else (Reject, 5)
    2 -> if c == '*' then (Wait, 3) else (Wait, 2)
    3 -> if c == ')' then (Accept Comment, 4) else (Wait, 3)
    _ -> (Reject, 5)

matchSpace :: StateMachine Int
matchSpace = StateMachine 0 $ \c -> state $ \case
    0 -> if c == ' ' || c == '\t' || c == '\n' then (Accept Whitespace, 0) else (Reject, 1)
    _ -> (Reject, 1)

stepStateMachine :: Char -> StateMachine a -> (MatchState, StateMachine a)
stepStateMachine inp (StateMachine s cont) = (output, StateMachine nextState cont)
    where (output, nextState) = runState (cont inp) s

stepStateMachineUnif :: Char -> AnyStateMachine -> (MatchState, AnyStateMachine)
stepStateMachineUnif inp = \case
    USM x -> let (a, res) = stepStateMachine inp x in (a, USM res)

getNextTokenImpl :: [AnyStateMachine] -> String -> [(Token, String)]
getNextTokenImpl rules = \case
    []        -> []
    (nx:rest) -> getNextTokenImpl nextRules rest ++ acceptResults
        where
            results = filter (\x -> fst x /= Reject) $ map (stepStateMachineUnif nx) rules
            acceptResults = mapMaybe (\x -> case fst x of { Accept t -> Just (t, rest); _ -> Nothing }) results
            nextRules = map snd results

getNextToken :: [AnyStateMachine] -> String -> Maybe (Token, String)
getNextToken rules input = case getNextTokenImpl rules input of
    []    -> Nothing
    (x:_) -> Just x

-- The magic function that gives you all of your Lumina tokens
getAllTokens :: [AnyStateMachine] -> String -> [Token]
getAllTokens rules input = case input of
    [] -> []
    _  -> case getNextToken rules input of
        -- TODO: More descriptive errors. We can track how far along the string we are fairly easily
        Nothing       -> error $ "Lexer error on token: " ++ [head input]
        Just (tok, s) -> tok : getAllTokens rules s

getAllTokensLumina :: String -> [Token]
getAllTokensLumina = getAllTokens tokenDefs

-- LUMINA TOKEN DEFINITIONS
-- Highest priority tokens are at the top.
-- TODO: Add minified versions of the tokens
tokenDefs :: [AnyStateMachine]
tokenDefs = [
    matchString_ Zero "0",
    matchString_ One "1",
    matchString_ LParen "(",
    matchString_ RParen ")",
    matchString_ Colon ":",
    matchString_ Semicolon ";",
    matchString_ Bang "!",
    matchString_ Assign ":=",
    matchString_ LessThan "<",
    matchString_ Ref "#",
    matchString_ IntType "int",
    matchString_ BoolType "bool",
    matchString_ UnitType "unit",
    matchString_ Add "+",
    matchString_ Sub "-",
    matchString_ Mul "*",
    matchString_ AndOp "&&",
    matchString_ OrOp "||",
    matchString_ Bar "|",
    matchString_ Arrow "->",
    matchString_ Not "~", -- TODO: Unify this token with ! and then resolve type checking
    matchString_ Equal "=",
    matchString_ Fun "fun",
    matchString_ With "with",
    matchString_ Do "do",
    matchString_ End "end",
    matchString_ While "while",
    matchString_ Case "case",
    USM matchAnyIdent,
    USM matchAnyPositiveInt,
    USM matchSpace,
    USM matchComment]
    where 
        matchString_ tok s = USM (matchString tok s)
