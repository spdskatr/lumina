module Lumina.Middleend.Shortcuts (optMonaProgram, astraToCelia) where

import Lumina.Middleend.Mona.Mona (MonaFunction (MonaFunction), MonaTranslationUnit, astraToMona)
import Lumina.Utils (untilFixedPoint)
import Lumina.Middleend.Mona.PropagateConsts (propagateConsts)
import Lumina.Middleend.Mona.OptimiseArith (optimiseArith)

import qualified Data.Map.Strict as Map
import Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode)
import Lumina.Middleend.Celia.Celia (CeliaTranslationUnit, monaToCelia)
import Lumina.Middleend.Astra.Astra (AST)

optMonaProgram :: MonaTranslationUnit -> MonaTranslationUnit
optMonaProgram = untilFixedPoint optMonaProgramImpl

optMonaProgramImpl :: MonaTranslationUnit -> MonaTranslationUnit
optMonaProgramImpl = Map.map optMona
    where
        toBody f (MonaFunction name fv x t t' e) = MonaFunction name fv x t t' (f e)
        optMona = elimDeadCode . toBody optimiseArith . toBody propagateConsts

astraToCelia :: AST -> CeliaTranslationUnit
astraToCelia = monaToCelia . optMonaProgram . astraToMona