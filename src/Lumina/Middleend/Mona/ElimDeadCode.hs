module Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode, elimDeadCodeImpl) where

import Lumina.Middleend.Mona.Mona (MExpr (..), MAtom (..), MOper (..), MonaFunction (MonaFunction))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (orElse)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(..))

type CaptureContext = Map String [String]

elimDeadCode :: CaptureContext -> MonaFunction -> MonaFunction
elimDeadCode cc (MonaFunction name fv x t t' body) = 
    let (body', vars) = elimDeadCodeImpl cc body
        fv' = filter (\(TypedVar y _) -> y `elem` vars) fv
    in MonaFunction name fv' x t t' body'

-- Determines if code can alter the state of the store
hasSideEffects :: MOper -> Bool
hasSideEffects (MApp _ _) = True
hasSideEffects _ = False

elimDeadCodeImpl :: CaptureContext -> MExpr -> (MExpr, [String])
elimDeadCodeImpl cc (MReturn a) = (MReturn a, matchVar cc a)
elimDeadCodeImpl cc (MAssign ma ma' me) = 
    let (me', vs) = elimDeadCodeImpl cc me
    in (MAssign ma ma' me', matchVar cc ma ++ matchVar cc ma' ++ vs)
elimDeadCodeImpl cc (MIf (MBool True) me1 _) = elimDeadCodeImpl cc me1
elimDeadCodeImpl cc (MIf (MBool False) _ me2) = elimDeadCodeImpl cc me2
elimDeadCodeImpl cc (MIf ma me1 me2) =
    let (me1', vs1) = elimDeadCodeImpl cc me1
        (me2', vs2) = elimDeadCodeImpl cc me2
    in (MIf ma me1' me2', matchVar cc ma ++ vs1 ++ vs2)
elimDeadCodeImpl cc (MLet s t mv me) =
    let (me', vs) = elimDeadCodeImpl cc me
    in if s `elem` vs || hasSideEffects mv then
        (MLet s t mv me', matchVal cc mv ++ [x | x <- vs, x /= s])
    else
        (me', vs)

matchVal :: CaptureContext -> MOper -> [String]
matchVal cc (MJust ma) = matchVar cc ma
matchVal cc (MUnary _ ma) = matchVar cc ma
matchVal cc (MBinary _ ma ma') = matchVar cc ma ++ matchVar cc ma'
matchVal cc (MApp ma ma') = matchVar cc ma ++ matchVar cc ma'

matchVar :: CaptureContext -> MAtom -> [String]
matchVar cc (MVar x) = x : (cc Map.!? x `orElse` [])
matchVar _ _ = []