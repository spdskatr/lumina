module Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode, elimDeadCodeImpl) where

import Lumina.Middleend.Mona.Mona (MExpr (..), MAtom (..), MOper (..), MonaFunction (MonaFunction))

import Lumina.Middleend.Astra.HoistFunctions (TypedVar(..))

elimDeadCode :: MonaFunction -> MonaFunction
elimDeadCode (MonaFunction name fv x t t' body) = 
    let (body', vars) = elimDeadCodeImpl body
        fv' = filter (\(TypedVar y _) -> y `elem` vars) fv
    in MonaFunction name fv' x t t' body'

-- Determines if code can alter the state of the store
hasSideEffects :: MOper -> Bool
hasSideEffects (MApp _ _) = True
hasSideEffects _ = False

elimDeadCodeImpl :: MExpr -> (MExpr, [String])
elimDeadCodeImpl (MReturn a) = (MReturn a, matchVar a)
elimDeadCodeImpl (MAssign ma ma' me) = 
    let (me', vs) = elimDeadCodeImpl me
    in (MAssign ma ma' me', matchVar ma ++ matchVar ma' ++ vs)
elimDeadCodeImpl (MIf (MBool True) me1 _) = elimDeadCodeImpl me1
elimDeadCodeImpl (MIf (MBool False) _ me2) = elimDeadCodeImpl me2
elimDeadCodeImpl (MIf ma me1 me2) =
    let (me1', vs1) = elimDeadCodeImpl me1
        (me2', vs2) = elimDeadCodeImpl me2
    in (MIf ma me1' me2', matchVar ma ++ vs1 ++ vs2)
elimDeadCodeImpl (MLet s t mv me) =
    let (me', vs) = elimDeadCodeImpl me
    in if s `elem` vs || hasSideEffects mv then
        (MLet s t mv me', matchVal mv ++ [x | x <- vs, x /= s])
    else
        (me', vs)

matchVal :: MOper -> [String]
matchVal (MMkClosure s vs) = s : concatMap matchVar vs
matchVal (MJust ma) = matchVar ma
matchVal (MUnary _ ma) = matchVar ma
matchVal (MBinary _ ma ma') = matchVar ma ++ matchVar ma'
matchVal (MApp ma ma') = matchVar ma ++ matchVar ma'

matchVar :: MAtom -> [String]
matchVar (MVar x) = [x]
matchVar _ = []