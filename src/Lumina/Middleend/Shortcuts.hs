module Lumina.Middleend.Shortcuts (transform, toOptContinuationForm) where

import Lumina.Middleend.EliminateEtaRedex (elimEta)
import Lumina.Middleend.CPSConvert (toCPS)
import Lumina.Frontend.LuminaAST (AST)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.GlobaliseFunctions (FunctionEnv, toContinuationForm)
import Lumina.Middleend.EliminateShadowing (elimShadowing)

transform :: AST -> AST
transform = untilFixedPoint elimEta . toCPS

-- Return an eta-redex eliminated Continuation Form.
-- Temporarily disabled because I still can't decide.
-- Eta-redexes are useful for the time being since they leave each function in
-- a nice form.
toOptContinuationForm :: AST -> FunctionEnv
toOptContinuationForm ast =
    let fs = toContinuationForm $ elimShadowing ast
    in fs