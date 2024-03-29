module Lumina.Middleend.Astra.Astra (
    TypeEnv,
    BinaryOp (..),
    UnaryOp (..),
    AST (..),
    astraType,
    toAST,
    isTrivial,
    freeVars,
    replaceVar,
    (>:=),
    (>>:=),
    (><>)
) where

import Lumina.Frontend.LuminaGrammar (PAST (..))
import Data.Map.Strict (Map)
import Lumina.Utils (internalError, fastNub, orElse, indent)
import Lumina.Frontend.Lexer (Token(..))
import Control.Monad (liftM2)
import Lumina.Middleend.Typing (LuminaType(..))

import qualified Data.Map.Strict as Map

type TypeEnv = Map String LuminaType

data BinaryOp
    = OpAdd
    | OpSub
    | OpMul
    | OpAnd
    | OpOr
    | OpLessThan
    | OpIntEqual
    | OpBoolEqual
    deriving Eq

instance Show BinaryOp where
    show OpAdd = "+"
    show OpSub = "-"
    show OpMul = "*"
    show OpAnd = "&&"
    show OpOr = "||"
    show OpLessThan = "<"
    show OpIntEqual = "=="
    show OpBoolEqual = "XNOR"

data UnaryOp
    = OpNot
    | OpBang
    | OpRef
    deriving Eq

instance Show UnaryOp where
    show OpNot = "~"
    show OpBang = "!"
    show OpRef = "#"

data AST
    = ABool Bool
    | AInt Int
    | AUnit
    | AVar String LuminaType
    | AApp AST AST LuminaType
    | AUnaryOp UnaryOp AST LuminaType
    | ABinaryOp BinaryOp AST AST LuminaType
    | AAssign AST AST
    | AIf AST AST AST LuminaType
    | AFun String LuminaType AST LuminaType
    | ALet String LuminaType AST AST LuminaType
    | ALetFun String String LuminaType AST AST LuminaType
    | ASeq AST AST LuminaType
    deriving (Eq)

type TranslationRes a = Either String a

astraType :: AST -> LuminaType
astraType (ABool _) = TBool
astraType (AInt _) = TInt
astraType AUnit = TUnit
astraType (AVar _ t) = t
astraType (AApp _ _ t) = t
astraType (AUnaryOp _ _ t) = t
astraType (ABinaryOp _ _ _ t) = t
astraType (AAssign _ _) = TUnit
astraType (AIf _ _ _ t) = t
astraType (AFun _ _ _ t) = t
astraType (ALet _ _ _ _ t) = t
astraType (ALetFun _ _ _ _ _ t) = t
astraType (ASeq _ _ t) = t

instance Show AST where
    show (ABool b) = show b
    show (AInt i) = show i
    show AUnit = "()"
    show (AVar s _) = s
    show (AApp a b _) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (AUnaryOp uo a _) = show uo ++ "(" ++ show a ++ ")"
    show (ABinaryOp bo a b _) = "(" ++ show a ++ " " ++ show bo ++ " " ++ show b ++ ")"
    show (AAssign a b) = show a ++ " := " ++ show b
    show (AIf a b c _) = "if " ++ show a ++ " then\n" ++ indent (show b) ++ "else\n" ++ indent (show c) ++ "end"
    show (ALet x t a b _) = "let " ++ x ++ " : " ++ show t ++ " = " ++ show a ++ " in\n" ++ indent (show b) ++ "end"
    show (AFun s t b _) = "fun " ++ s ++ " : " ++ show t ++ " ->\n" ++ indent (show b) ++ "end"
    show (ALetFun f x t a b _) = "let fun " ++ f ++ " (" ++ x ++ " : " ++ show t ++ ") =\n" ++ indent (show a) ++ "in\n" ++ indent (show b) ++ "end"
    show (ASeq a b _) = show a ++ ";\n" ++ show b

-- Rename all references to a variable within an AST to something else.
replaceVar :: String -> AST -> AST -> AST
replaceVar x r = (findVar >:=)
    where
        findVar ast = case ast of
            AVar y _ | x == y             -> Just r
            AFun y _ _ _ | x == y         -> Just ast
            ALet y t a b t' | x == y      -> Just (ALet y t (replaceVar x r a) b t')
            ALetFun f _ _ _ _ _ | x == f  -> Just ast
            ALetFun f y t a b t' | x == y -> Just (ALetFun f y t a (replaceVar x r b) t')
            _ -> Nothing

isTrivial :: AST -> Bool
isTrivial ast = case ast of
    ABool _ -> True
    AInt _ -> True
    AUnit -> True
    AVar _ _ -> True
    _ -> False

-- Recursively transform the AST by providing a pattern match procedure.
-- The recursion stops as soon as an update is found.
(>:=) :: (AST -> Maybe AST) -> AST -> AST
f >:= ast = f ast `orElse` case ast of
    ABool _ -> ast
    AInt _ -> ast
    AUnit -> ast
    AVar _ _ -> ast
    AApp ast' ast2 t -> AApp (f >:= ast') (f >:= ast2) t
    AUnaryOp uo ast' t -> AUnaryOp uo (f >:= ast') t
    ABinaryOp bo ast' ast2 t -> ABinaryOp bo (f >:= ast') (f >:= ast2) t
    AAssign ast' ast2 -> AAssign (f >:= ast') (f >:= ast2)
    AIf ast' ast2 ast3 t -> AIf (f >:= ast') (f >:= ast2) (f >:= ast3) t
    AFun s t ast' t' -> AFun s t (f >:= ast') t'
    ALet s t ast' ast2 t' -> ALet s t (f >:= ast') (f >:= ast2) t'
    ALetFun s str t ast' ast2 t' -> ALetFun s str t (f >:= ast') (f >:= ast2) t'
    ASeq ast' ast2 t -> ASeq (f >:= ast') (f >:= ast2) t

-- Distribute a monadic action over a single level of the AST.
(>>:=) :: (Monad m) => (AST -> m AST) -> AST -> m AST
f >>:= ast = case ast of
    AApp ast1 ast2 t -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (AApp ast1' ast2' t)
    AUnaryOp uo ast1 t -> do
        ast1' <- f ast1
        return (AUnaryOp uo ast1' t)
    ABinaryOp bo ast1 ast2 t -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (ABinaryOp bo ast1' ast2' t)
    AAssign ast1 ast2 -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (AAssign ast1' ast2')
    AIf ast1 ast2 ast3 t -> do
        ast1' <- f ast1
        ast2' <- f ast2
        ast3' <- f ast3
        return (AIf ast1' ast2' ast3' t)
    AFun s t ast1 t' -> do
        ast1' <- f ast1
        return (AFun s t ast1' t')
    ALet s t ast1 ast2 t' -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (ALet s t ast1' ast2' t')
    ALetFun s str t ast1 ast2 t' -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (ALetFun s str t ast1' ast2' t')
    ASeq ast1 ast2 t -> do
        ast1' <- f ast1
        ast2' <- f ast2
        return (ASeq ast1' ast2' t)
    _ -> return ast

-- Fold over a single level of the AST.
(><>) :: (Monoid m) => (AST -> m) -> AST -> m
f ><> ast = case ast of
    ABool _ -> mempty
    AInt _ -> mempty
    AUnit -> mempty
    AVar _ _ -> mempty
    AApp ast' ast2 _ -> f ast' <> f ast2
    AUnaryOp _ ast' _ -> f ast'
    ABinaryOp _ ast' ast2 _ -> f ast' <> f ast2
    AAssign ast' ast2 -> f ast' <> f ast2
    AIf ast' ast2 ast3 _ -> f ast' <> f ast2 <> f ast3
    AFun _ _ ast' _ -> f ast'
    ALet _ _ ast' ast2 _ -> f ast' <> f ast2
    ALetFun _ _ _ ast' ast2 _ -> f ast' <> f ast2
    ASeq ast' ast2 _ -> f ast' <> f ast2

typeError :: String -> TranslationRes a
typeError s = Left $ "Type error: " ++ s

applyType :: LuminaType -> LuminaType -> TranslationRes LuminaType
applyType t1 t2 = case t1 of
    TFun t2' t3 | t2 == t2' -> return t3
    _ -> typeError ("Unmatched types " ++ show t1 ++ " and " ++ show t2)

checkInts :: LuminaType -> LuminaType -> TranslationRes LuminaType
checkInts t1 t2 = case (t1, t2) of
    (TInt, TInt) -> return TInt
    _ -> typeError ("Arguments passed to arithmetic operation must be of integer type; found " ++ show (t1, t2) ++ " instead")

checkBools :: LuminaType -> LuminaType -> TranslationRes LuminaType
checkBools t1 t2 = case (t1, t2) of
    (TBool, TBool) -> return TBool
    _ -> typeError ("Arguments passed to boolean operation must be of boolean type; found " ++ show (t1, t2) ++ " instead")

checkComparison :: LuminaType -> LuminaType -> TranslationRes LuminaType
checkComparison t1 t2 = case (t1, t2) of
    (TInt, TInt) -> return TBool
    _ -> typeError ("Arguments passed to comparison must be of integer type; found " ++ show (t1, t2) ++ " instead")

checkAssign :: LuminaType -> LuminaType -> TranslationRes LuminaType
checkAssign t1 t2 = case t1 of
    (TRef t2') | t2' == t2 -> return TUnit
    _ -> typeError ("Expecting " ++ show (TRef t2) ++ " in assignment; found " ++ show t1 ++ " instead")

getType :: PAST -> TranslationRes LuminaType
getType PTInt = return TInt
getType PTBool = return TBool
getType PTUnit = return TUnit
getType (PTRef x) = TRef <$> getType x
getType (PTFun x y) = liftM2 TFun (getType x) (getType y)
getType x = typeError ("Expression is not a type: " ++ show x)

getEqualsOp :: LuminaType -> LuminaType -> Maybe BinaryOp
getEqualsOp TInt TInt = Just OpIntEqual
getEqualsOp TBool TBool = Just OpBoolEqual
getEqualsOp _ _ = Nothing

makeCases :: (String, LuminaType) -> TypeEnv -> [(PAST, PAST)] -> TranslationRes (AST, LuminaType)
makeCases _ _ [] = typeError "No matches in case expression"
makeCases (s,t) env [(pa,pb)] =
    case pa of
        PVar (PToken (Ident x)) -> do
            let newEnv = Map.insert x t env
            (a2,t2) <- translate newEnv pb
            return (replaceVar x (AVar s t) a2, t2)
        _ -> do
            typeError "Case expression should always end with a default case"
makeCases (s,t) env ((pa,pb):rest) = do
    (a1,t1) <- translate env pa
    (a2,t2) <- translate env pb
    (aa,t') <- makeCases (s,t) env rest
    case getEqualsOp t t1 of
        Nothing -> typeError ("Case split expecting equatable types, got types " ++ show (t, t1) ++ " instead")
        Just op -> if t2 /= t' then 
            typeError ("Result types of case split must be equal, got types " ++ show (t2, t') ++ " instead")
        else 
            return (AIf (ABinaryOp op (AVar s t) a1 TBool) a2 aa t2, t2)

translate :: TypeEnv -> PAST -> TranslationRes (AST, LuminaType)
translate env past = case past of
    PToken _ -> internalError "Found lonely token"
    PZero -> return (AInt 0, TInt)
    PTrue -> return (ABool True, TBool)
    PFalse -> return (ABool False, TBool)
    PUnit -> return (AUnit, TUnit)
    PVar (PToken (Ident x)) -> do
        case Map.lookup x env of
            Nothing -> typeError ("Unbound variable " ++ x)
            Just t -> return (AVar x t, t)
    PInt (PToken (IntLit x)) -> return (AInt x, TInt)
    PApp pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- applyType t1 t2
        return (AApp a1 a2 t, t)
    PBang pa -> do
        (a1, t1) <- translate env pa
        case t1 of
            TRef x -> return (AUnaryOp OpBang a1 x, x)
            TBool -> return (AUnaryOp OpNot a1 TBool, TBool)
            _ -> typeError ("Mismatched type for ! (should be bool or reference): " ++ show t1)
    PRef pa -> do
        (a1, t1) <- translate env pa
        return (AUnaryOp OpRef a1 (TRef t1), TRef t1)
    PMul pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpMul a1 a2 t, t)
    PSub pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpSub a1 a2 t, t)
    PAdd pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkInts t1 t2
        return (ABinaryOp OpAdd a1 a2 t, t)
    PAnd pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkBools t1 t2
        return (ABinaryOp OpAnd a1 a2 t, t)
    POr pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkBools t1 t2
        return (ABinaryOp OpOr a1 a2 t, t)
    PLessThan pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkComparison t1 t2
        return (ABinaryOp OpLessThan a1 a2 t, t)
    PEqual pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        let eqOp = getEqualsOp t1 t2
        case eqOp of 
            Just x -> return (ABinaryOp x a1 a2 TBool, TBool)
            Nothing -> typeError ("Expected equatable types for = comparison; got " ++ show (t1, t2) ++ " instead")
    PAssign pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,t2) <- translate env pa'
        t <- checkAssign t1 t2
        return (AAssign a1 a2, t)
    PWhile pa pa' -> do
        (a1,t1) <- translate env pa
        (a2,_) <- translate env pa'
        if t1 == TBool then 
            return (ALetFun "0while" "0cond" TBool (
                    AIf (AVar "0cond" TBool) (
                        ASeq a2 (AApp (
                            AVar "0while" (TFun TBool TUnit)) a1 
                        TUnit)
                    TUnit) 
                AUnit TUnit) (
                    AApp (AVar "0while" (TFun TBool TUnit)) a1 TUnit
                ) TUnit, TUnit)
        else 
            typeError ("Expected boolean type for expression to while; got " ++ show t1 ++ " instead")
    PSeq pa pa' -> do
        (a1,_) <- translate env pa
        (a2,t2) <- translate env pa'
        return (ASeq a1 a2 t2, t2)
    PFun (PToken (Ident x)) ta ta' pa' -> do
        inType <- getType ta
        let newEnv = Map.insert x inType env
        (a1,t1) <- translate newEnv pa'
        t2 <- getType ta'
        if t1 == t2 then 
            let newType = TFun inType t2
            in return (AFun x inType a1 newType, newType)
        else
            typeError ("Expected type " ++ show t2 ++ " in lambda, got " ++ show t1 ++ " instead")
    PLet (PToken (Ident x)) ta pa pa' -> do
        actualType <- getType ta
        let newEnv = Map.insert x actualType env
        (a1,t1) <- translate env pa
        (a2,t2) <- translate newEnv pa'
        if t1 == actualType then
            return (ALet x actualType a1 a2 t2, t2)
        else
            typeError ("Expected type " ++ show actualType ++ " in with, got " ++ show t1 ++ " instead")
    
    -- with fun f (x : A) -> B = a' do b' end
    -- with f : (A -> B) = fun (x : A) : B -> a' end do b' end
    PLetFun (PToken (Ident f)) (PToken (Ident x)) ta ta' pa' pb -> do
        inType <- getType ta
        outType <- getType ta'
        let newEnvB = Map.insert f (TFun inType outType) env
        let newEnvA = Map.insert x inType newEnvB
        (a1,t1) <- translate newEnvA pa'
        (a2,t2) <- translate newEnvB pb
        if t1 == outType then
            return (ALetFun f x inType a1 a2 t2, t2)
        else
            typeError ("Expected type " ++ show outType ++ " in body of function " ++ f ++ ", got " ++ show t1 ++ " instead")
    PCase pa (PCaseList l) -> do
        (a1,t1) <- translate env pa
        let innerCaseEnv = Map.insert "0case" t1 env
        (a2,t2) <- makeCases ("0case",t1) innerCaseEnv l
        return (ALet "0case" t1 a1 a2 t2, t2)
    PCaseList _ -> internalError "Found lonely CaseList"
    _ -> internalError $ "Bad PAST: " ++ show past

toAST :: PAST -> (AST, LuminaType)
toAST past = case translate Map.empty past of
    Left s -> error s
    Right a -> a

freeVars :: AST -> [String]
freeVars = fastNub . freeVarsImpl
    where
        freeVarsImpl (AVar x _) = [x]
        freeVarsImpl (AFun s _ x _) = filter (/= s) $ freeVars x
        freeVarsImpl (ALet x _ s t _) = freeVars s ++ filter (/= x) (freeVars t)
        freeVarsImpl (ALetFun f x _ s t _) = 
            let a1 = filter (\y -> y /= x && y /= f) $ freeVars s
                a2 = filter (/= f) $ freeVars t
            in a1 ++ a2
        freeVarsImpl ast = freeVarsImpl ><> ast