module Lumina.Middleend.Shortcuts (transform, toOptContinuationForm) where

import Lumina.Middleend.EliminateEtaRedex (elimEta, elimEtaContForm)
import Lumina.Middleend.CPSConvert (toCPS)
import Lumina.Frontend.LuminaAST (AST)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.GlobaliseFunctions (FunctionEnv, toContinuationForm)

import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor

transform :: AST -> AST
transform = untilFixedPoint elimEta . toCPS

-- Return an eta-redex eliminated Continuation Form.
toOptContinuationForm :: AST -> FunctionEnv
toOptContinuationForm ast =
    let fs = toContinuationForm ast
    in Map.map (Bifunctor.second $ untilFixedPoint (elimEtaContForm fs)) fs