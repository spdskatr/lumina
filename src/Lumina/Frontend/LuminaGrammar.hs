{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lumina.Frontend.LuminaGrammar (
    LNT(..),
    PAST(..),
    luminaAnnotatedGrammar,
    luminaGrammar,
    luminaReduceActions
) where

import Lumina.Frontend.Lexer (Tag, TokenTag (..), Token)
import Lumina.Frontend.ParserGen (Production (..), NonTerminal (..), GrammarSymbol (..), Terminal (..))
import Lumina.Utils (internalError)
import Data.Ix (Ix)

data LNT
    = Start
    | Expr
    | PExpr
    | QExpr
    | RExpr
    | AExpr
    | BExpr
    | CExpr
    | Atom
    | TExpr
    | TSubExpr
    | EndBlock
    | CaseList deriving (Show, Bounded, Eq, Enum, Ord, Ix, Read)

instance Tag LNT

data PAST
    = PToken Token
    | PZero
    | PTrue
    | PFalse
    | PUnit
    | PVar PAST
    | PInt PAST
    | PApp PAST PAST
    | PNot PAST
    | PBang PAST
    | PRef PAST
    | PMul PAST PAST
    | PAnd PAST PAST
    | POr PAST PAST
    | PSub PAST PAST
    | PAdd PAST PAST
    | PLessThan PAST PAST
    | PEqual PAST PAST
    | PAssign PAST PAST
    | PCase PAST PAST
    | PCaseList [(PAST, PAST)]
    | PFun PAST PAST PAST PAST
    | PLet PAST PAST PAST PAST
    | PLetFun PAST PAST PAST PAST PAST PAST
    | PWhile PAST PAST
    | PSeq PAST PAST
    | PTInt
    | PTBool
    | PTUnit
    | PTRef PAST
    | PTFun PAST PAST
    deriving (Eq, Show)

-- Utils for making grammars
class GrammarSymbolSeq s where
    pr' :: [GrammarSymbol LNT TokenTag] -> LNT -> s

instance GrammarSymbolSeq (Production LNT TokenTag) where
    pr' gs nt = Production (NonTerminal nt) (reverse gs)

instance (GrammarSymbolSeq s) => GrammarSymbolSeq (LNT -> s) where
    pr' symbs nt tt = pr' (NTSymb (NonTerminal tt) : symbs) nt

instance (GrammarSymbolSeq s) => GrammarSymbolSeq (TokenTag -> s) where
    pr' symbs nt tt = pr' (TSymb (Tok tt) : symbs) nt

p_ :: (GrammarSymbolSeq s) => LNT -> s
p_ = pr' []
-- End of syntax magic

luminaGrammar :: [Production LNT TokenTag]
luminaGrammar = map snd luminaAnnotatedGrammar

luminaReduceActions :: [[(Int, PAST)] -> PAST]
luminaReduceActions = map fst luminaAnnotatedGrammar

----------------------
-- Notation:
-- A -> BcD
--   is the same as
-- p_ A B c D

(!) :: [(a, b)] -> Int -> b
l ! i = snd $ l !! i

luminaAnnotatedGrammar :: [([(Int, PAST)] -> PAST, Production LNT TokenTag)]
luminaAnnotatedGrammar = [
    -- Start
    (,) (const $ internalError "Cannot reduce Start nonterminal") $
    p_ Start Expr,

    -- TExpr (Types)
    (,) (const PTInt) $
    p_ TExpr IntTypeT,

    (,) (const PTUnit) $
    p_ TExpr UnitTypeT,

    (,) (const PTBool) $
    p_ TExpr BoolTypeT,

    (,) (\[(_,a),_] -> PTRef a) $
    p_ TExpr TExpr RefT,

    (,) (\[_,(_,a),_] -> a) $
    p_ TExpr LParenT TSubExpr RParenT,

    -- TSubExpr (Add arrows)
    (,) (\[(_,a)] -> a) $
    p_ TSubExpr TExpr,

    (,) (\[(_,a),_,(_,b)] -> PTFun a b) $
    p_ TSubExpr TExpr ArrowT TSubExpr,

    -- Expr (Sequencing)
    (,) (\[(_,a)] -> a) $
    p_ Expr PExpr,

    (,) (\[(_,a),_,(_,b)] -> PSeq a b) $
    p_ Expr Expr SemicolonT PExpr,

    -- PExpr (Assignment)
    (,) (\[(_,a)] -> a) $
    p_ PExpr QExpr,

    (,) (\[(_,a),_,(_,c)] -> PAssign a c) $
    p_ PExpr QExpr AssignT PExpr,

    -- QExpr (Comparison)
    (,) (\[(_,a)] -> a) $
    p_ QExpr RExpr,
    
    (,) (\[(_,a),_,(_,c)] -> PLessThan a c) $
    p_ QExpr RExpr LessThanT QExpr,
    
    (,) (\[(_,a),_,(_,c)] -> PEqual a c) $
    p_ QExpr RExpr EqualT QExpr,

    -- RExpr (Boolean Operators)
    (,) (\[(_,a)] -> a) $
    p_ RExpr AExpr,
    
    (,) (\[(_,a),_,(_,c)] -> POr a c) $
    p_ RExpr AExpr OrOpT RExpr,

    (,) (\[(_,a),_,(_,c)] -> PAnd a c) $
    p_ RExpr AExpr AndOpT RExpr,

    -- AExpr (Add/Subtract Arithmetic)
    (,) (\[(_,a)] -> a) $
    p_ AExpr BExpr,

    (,) (\[(_,a),_,(_,b)] -> PAdd a b) $
    p_ AExpr AExpr AddT BExpr,

    (,) (\[(_,a),_,(_,b)] -> PSub a b) $
    p_ AExpr AExpr SubT BExpr,

    -- BExpr (Multiplication)
    (,) (\[(_,a)] -> a) $
    p_ BExpr CExpr,

    (,) (\[(_,a),_,(_,b)] -> PMul a b) $
    p_ BExpr BExpr MulT CExpr,

    -- CExpr (Unary)
    (,) (\[(_,a)] -> a) $
    p_ CExpr Atom,

    (,) (\[_,(_,a)] -> PNot a) $
    p_ CExpr NotT Atom,

    (,) (\[_,(_,a)] -> PBang a) $
    p_ CExpr BangT Atom,

    (,) (\[_,(_,a)] -> PRef a) $
    p_ CExpr RefT Atom,

    -- CExpr (Application)
    (,) (\[(_,a),(_,b)] -> PApp a b) $
    p_ CExpr CExpr Atom,

    -- CaseList
    (,) (\l -> PCaseList [(l!1,l!3)]) $
    p_ CaseList BarT Expr ArrowT Expr EndBlock,

    (,) (\[_,(_,c),_,(_,e),(_,PCaseList l)] -> PCaseList ((c,e):l)) $
    p_ CaseList BarT Expr ArrowT Expr CaseList,

    -- Atom
    (,) (\l -> PCase (l!1) (l!3)) $
    p_ Atom WithT Expr CaseT CaseList,

    (,) (\l -> PFun (l!2) (l!4) (l!7) (l!9)) $
    p_ Atom FunT LParenT IdentT ColonT TExpr RParenT ColonT TExpr ArrowT Expr EndBlock,

    (,) (\l -> PLet (l!1) (l!3) (l!5) (l!7)) $
    p_ Atom WithT IdentT ColonT TExpr EqualT Expr DoT Expr EndBlock,

    (,) (\l -> PLetFun (l!2) (l!4) (l!6) (l!9) (l!11) (l!13)) $
    p_ Atom WithT FunT IdentT LParenT IdentT ColonT TExpr RParenT ColonT TExpr EqualT Expr DoT Expr EndBlock,

    (,) (\l -> PWhile (l!1) (l!3)) $
    p_ Atom WhileT Expr DoT Expr EndBlock,

    (,) (\[_,(_,a),_] -> a) $
    p_ Atom LParenT Expr RParenT,

    (,) (\[(_,a)] -> PInt a) $
    p_ Atom IntLitT,

    (,) (const PTrue) $
    p_ Atom TrueLitT,

    (,) (const PFalse) $
    p_ Atom FalseLitT,

    (,) (const PUnit) $
    p_ Atom UnitLitT,

    (,) (const PZero) $
    p_ Atom ZeroT,

    (,) (\[(_,a)] -> PVar a) $
    p_ Atom IdentT,

    -- EndBlock (To absorb semicolons)
    (,) (\[_] -> PTUnit) $
    p_ EndBlock EndT,

    (,) (\[_,_] -> PTUnit) $
    p_ EndBlock SemicolonT EndT
    ]