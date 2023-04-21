module Lumina.Middleend.Mona.ElimDeadCode (elimDeadCode, elimDeadCodeImpl) where

import Lumina.Middleend.Mona.Mona (MExpr (..), MAtom (..), MOper (..), MonaFunction (MonaFunction))

import Lumina.Middleend.Astra.HoistFunctions (TypedVar(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (internalError)

elimDeadCode :: Map String [TypedVar] -> MonaFunction -> MonaFunction
elimDeadCode sigs (MonaFunction name fv x t t' body) =
    let (body', vars) = elimDeadCodeImpl sigs body
        fv' = filter (\(TypedVar y _) -> y `elem` vars) fv
    in MonaFunction name fv' x t t' body'

-- Determines if code can alter the state of the store
hasSideEffects :: MOper -> Bool
hasSideEffects (MApp _ _) = True
hasSideEffects (MCall {}) = True
hasSideEffects _ = False

elimDeadCodeImpl :: Map String [TypedVar] -> MExpr -> (MExpr, [String])
elimDeadCodeImpl _ (MReturn a) = (MReturn a, matchVar a)
elimDeadCodeImpl sigs (MAssign ma ma' me) =
    let (me', vs) = elimDeadCodeImpl sigs me
    in (MAssign ma ma' me', matchVar ma ++ matchVar ma' ++ vs)
elimDeadCodeImpl sigs (MIf (MBool True) me1 _) = elimDeadCodeImpl sigs me1
elimDeadCodeImpl sigs (MIf (MBool False) _ me2) = elimDeadCodeImpl sigs me2
elimDeadCodeImpl sigs (MIf ma me1 me2) =
    let (me1', vs1) = elimDeadCodeImpl sigs me1
        (me2', vs2) = elimDeadCodeImpl sigs me2
    in (MIf ma me1' me2', matchVar ma ++ vs1 ++ vs2)
elimDeadCodeImpl sigs (MLetInline s t me nx) =
    let (nx', vs1) = elimDeadCodeImpl sigs nx
        (me', vs2) = elimDeadCodeImpl sigs me
    in (MLetInline s t me' nx', vs1 ++ vs2)
elimDeadCodeImpl sigs (MLet s t mv me) =
    let (me', vs) = elimDeadCodeImpl sigs me
    in if s `elem` vs || hasSideEffects mv then
        let mv' = elimDeadCodeOp sigs mv
        in (MLet s t mv' me', matchVal mv' ++ [x | x <- vs, x /= s])
    else
        (me', vs)

requiredVars :: [TypedVar] -> [(String, MAtom)] -> [(String, MAtom)]
requiredVars tvs = filter (\(x,_) -> any (\(TypedVar s _) -> s == x) tvs)

elimDeadCodeOp :: Map String [TypedVar] -> MOper -> MOper
elimDeadCodeOp sigs (MMkClosure f args) = case sigs Map.!? f of
    Just tvs -> MMkClosure f (requiredVars tvs args)
    Nothing -> internalError ("Function " ++ f ++ " not found")
elimDeadCodeOp sigs (MCall f env arg) = case sigs Map.!? f of
    Just tvs -> MCall f (requiredVars tvs env) arg
    Nothing -> internalError ("Function " ++ f ++ " not found")
elimDeadCodeOp _ x = x

matchVal :: MOper -> [String]
matchVal (MMkClosure s vs) = s : concatMap (matchVar . snd) vs
matchVal (MJust ma) = matchVar ma
matchVal (MUnary _ ma) = matchVar ma
matchVal (MBinary _ ma ma') = matchVar ma ++ matchVar ma'
matchVal (MApp ma ma') = matchVar ma ++ matchVar ma'
matchVal (MCall _ vs a) = matchVar a ++ concatMap (matchVar . snd) vs

matchVar :: MAtom -> [String]
matchVar (MVar x) = [x]
matchVar _ = []