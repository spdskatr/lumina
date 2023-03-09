{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleInstances #-}

module Lumina.Frontend.LuminaGrammar (
    LNT(..),
    luminaGrammar
) where

import Lumina.Frontend.Lexer (Tag, Taggable(..), Token(..), TokenTag(..))
import Lumina.Frontend.ParserGen (Production(..), NonTerminal(..), GrammarSymbol(..), Terminal(..),  LR1Item(..), LR0Item(..))
import Data.Ix (Ix)

data LNT
    = Start
    | Expr
    | AExpr
    | BExpr
    | CExpr
    | Atom
    | TExpr
    | CaseList deriving (Show, Bounded, Eq, Enum, Ord, Ix, Read)

instance Tag LNT

-- Lumina.Utils for making grammars
class GrammarSymbolSeq s where
    pr' :: [GrammarSymbol LNT TokenTag] -> LNT -> s

instance GrammarSymbolSeq (Production LNT TokenTag) where
    pr' gs nt = Production (NonTerminal nt) (reverse gs)

instance (GrammarSymbolSeq s) => GrammarSymbolSeq (LNT -> s) where
    pr' symbs nt tt = pr' ((NTSymb (NonTerminal tt)) : symbs) nt

instance (GrammarSymbolSeq s) => GrammarSymbolSeq (TokenTag -> s) where
    pr' symbs nt tt = pr' ((TSymb (Tok tt)) : symbs) nt

p_ :: (GrammarSymbolSeq s) => LNT -> s
p_ = pr' []
-- End of syntax magic

luminaGrammar :: [Production LNT TokenTag]
luminaGrammar = [
    -- Start
    p_ Start Expr,
    -- TExpr
    p_ TExpr IntTypeT,
    p_ TExpr UnitTypeT,
    p_ TExpr BoolTypeT,
    p_ TExpr TExpr RefT,
    -- Expr
    p_ Expr AExpr,
    p_ Expr Expr SemicolonT AExpr,
    -- AExpr
    p_ AExpr BExpr,
    p_ AExpr AExpr AddT BExpr,
    p_ AExpr AExpr SubT BExpr,
    p_ AExpr AExpr OrOpT BExpr,
    -- BExpr
    p_ BExpr CExpr,
    p_ BExpr BExpr MulT CExpr,
    p_ BExpr BExpr AndOpT CExpr,
    -- CExpr
    p_ CExpr Atom,
    p_ CExpr NotT Atom,
    p_ CExpr BangT Atom,
    p_ CExpr RefT Atom,
    p_ CExpr CExpr Atom,
    -- Atom
    p_ Atom FunT LParenT IdentT ColonT TExpr RParenT ColonT TExpr ArrowT Expr EndT,
    p_ Atom WithT IdentT ColonT TExpr EqualT Expr DoT Expr EndT,
    p_ Atom WithT IdentT ColonT LParenT IdentT ColonT TExpr RParenT ColonT TExpr EqualT Expr DoT Expr EndT,
    p_ Atom WhileT Expr DoT Expr EndT,
    p_ Atom LParenT Expr RParenT,
    p_ Atom IntLitT,
    p_ Atom OneT,
    p_ Atom ZeroT
    ]