module Lumina.Interpreter.MonaInterpreter (getMonaValue) where
import Lumina.Middleend.Mona.Mona (MExpr (..), MAtom (..), MonaTranslationUnit, MonaFunction (..), MOper (..))

import qualified Data.Map.Strict as Map
import Lumina.Utils (internalError, orElse)
import Lumina.Interpreter.AstraInterpreter (Value (..), Env, Store, applyUnaryOp, applyBinaryOp)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Lumina.Middleend.Astra.HoistFunctions (TypedVar(TypedVar))

getAtomValue :: Env -> MAtom -> Value
getAtomValue _ MUnit = VUnit
getAtomValue _ (MInt i) = VInt i
getAtomValue _ (MBool b) = VBool b
getAtomValue e (MVar x) = 
    case e Map.!? x `orElse` internalError ("Variable " ++ show x ++ " not found") of
        VClosure cl -> VFun (cl e)
        v -> v

getOperValue :: Env -> MOper -> State Store Value
getOperValue env (MJust a) = return $ getAtomValue env a
getOperValue env (MApp a b) = case getAtomValue env a of
    VFun f -> f (getAtomValue env b)
    v -> internalError ("Left side of application is not a function, found " ++ show v ++ " instead")
getOperValue env (MUnary uo a) = applyUnaryOp uo (getAtomValue env a)
getOperValue env (MBinary bo a b) = applyBinaryOp bo (getAtomValue env a) (getAtomValue env b)

interpretMona :: Env -> MExpr -> State Store Value
interpretMona env (MLet x _ o rest) = do
    v <- getOperValue env o
    interpretMona (Map.insert x v env) rest
interpretMona env (MAssign x a rest) = do
    let v1 = getAtomValue env x
        v2 = getAtomValue env a
    case v1 of
        VRef ad -> do
            store <- get
            put $ Map.insert ad v2 store
        _ -> internalError $ "Left side of assignment-expression did not evaluate to a reference, found " ++ show v1 ++ " instead"
    interpretMona env rest
interpretMona env (MReturn a) = return (getAtomValue env a)
interpretMona env (MIf a th el) = do
    let v = getAtomValue env a
    case v of
        VBool True -> interpretMona env th
        VBool False -> interpretMona env el
        _ -> internalError $ "If statement wants boolean as condition, found " ++ show v ++ " instead"

makeEnv :: MonaTranslationUnit -> Env
makeEnv fs = Map.map (\(MonaFunction _ fv x _ _ e) -> VClosure $ \outerEnv v -> 
    let boundFromOuter = Map.fromList $ (\(TypedVar k _) -> (k, outerEnv Map.!? k `orElse` unboundError k)) <$> fv
        boundMap = Map.union boundFromOuter (makeEnv fs)
        unboundError k = internalError ("variable " ++ k ++ " not found in outer context: " ++ show (Map.keys outerEnv) ++ " " ++ show v)
    in interpretMona (Map.insert x v boundMap) e) fs

getMonaValue :: MonaTranslationUnit -> Value
getMonaValue ctx = case ctx Map.!? "0main" of
    Just m -> 
        let env = makeEnv ctx
            st = interpretMona (Map.insert (getArg m) VUnit env) (getBody m)
        in evalState st Map.empty
    Nothing -> internalError "No main function found in translation unit; could not start interpreter"