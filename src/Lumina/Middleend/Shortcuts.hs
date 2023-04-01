module Lumina.Middleend.Shortcuts (transform, toOptContinuationForm) where

import Lumina.Middleend.Astra.CPS (toCPS)
import Lumina.Middleend.Astra.Astra (AST)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.Astra.HoistFunctions (FunctionEnv, toContinuationForm)
import Lumina.Middleend.Astra.ElimShadowing (elimShadowing)

transform :: AST -> AST
transform = toCPS

-- Return an eta-redex eliminated Continuation Form.
-- Temporarily disabled because I still can't decide.
-- Eta-redexes are useful for the time being since they leave each function in
-- a nice form.
toOptContinuationForm :: AST -> FunctionEnv
toOptContinuationForm ast =
    let fs = toContinuationForm $ elimShadowing ast
    in fs