module Lumina.Middleend.Shortcuts (optMonaProgram) where

import Lumina.Middleend.Mona.Mona (MonaFunction (MonaFunction), MonaTranslationUnit)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.Mona.PropagateConsts (propagateConsts)
import Lumina.Middleend.Mona.OptimiseArith (optimiseArith)

import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(TypedVar))

optMonaProgram :: MonaTranslationUnit -> MonaTranslationUnit
optMonaProgram = untilFixedPoint optMonaProgramImpl

optMonaProgramImpl :: MonaTranslationUnit -> MonaTranslationUnit
optMonaProgramImpl fs = Map.map optMona fs
    where
        toBody f (MonaFunction name fv x t t' e) = MonaFunction name fv x t t' (f e)
        optMona = elimDeadCode captureContext . toBody optimiseArith . toBody propagateConsts
        captureContext = Map.map (\(MonaFunction _ fv _ _ _ _) -> map (\(TypedVar x _) -> x) fv) fs