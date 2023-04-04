module Lumina.Middleend.Shortcuts (optMona, optMonaProgram) where

import Lumina.Middleend.Mona.Mona (MExpr, MonaFunction (MonaFunction))
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.Mona.PropagateConsts (propagateConsts)
import Lumina.Middleend.Mona.OptimiseArith (optimiseArith)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode)

optMona :: MExpr -> MExpr
optMona = untilFixedPoint (elimDeadCode . optimiseArith . propagateConsts)

optMonaProgram :: Map String MonaFunction -> Map String MonaFunction
optMonaProgram = Map.map $ \(MonaFunction f fv x body) ->
    MonaFunction f fv x (optMona body)