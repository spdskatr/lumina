module Lumina.Middleend.Shortcuts (optMonaProgram) where

import Lumina.Middleend.Mona.Mona (MonaFunction (MonaFunction), MonaTranslationUnit)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.Mona.PropagateConsts (propagateConsts)
import Lumina.Middleend.Mona.OptimiseArith (optimiseArith)

import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(TypedVar))


optMonaProgram :: MonaTranslationUnit -> MonaTranslationUnit
optMonaProgram fs = Map.map (\(MonaFunction f fv x t t' body) -> MonaFunction f fv x t t' (optMona body)) fs
    where
        optMona = untilFixedPoint (elimDeadCode captureContext . optimiseArith . propagateConsts)
        captureContext = Map.map (\(MonaFunction _ fv _ _ _ _) -> map (\(TypedVar x _) -> x) fv) fs