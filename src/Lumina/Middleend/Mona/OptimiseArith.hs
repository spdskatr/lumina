module Lumina.Middleend.Mona.OptimiseArith (optimiseArith) where
import Lumina.Middleend.Mona.Mona (MExpr (..), (>:=), MOper (..), MAtom (..))
import Lumina.Middleend.Astra.Astra (UnaryOp (..), BinaryOp (..))

justAtom :: MAtom -> Maybe MOper
justAtom = Just . MJust

optNot :: MAtom -> Maybe MOper
optNot (MBool b) = justAtom (MBool (not b))
optNot _ = Nothing

optBoolOp :: BinaryOp -> Bool -> MAtom -> Maybe MOper
optBoolOp OpAnd True e = justAtom e
optBoolOp OpAnd False _ = justAtom (MBool False)
optBoolOp OpOr True _ = justAtom (MBool True)
optBoolOp OpOr False e = justAtom e
optBoolOp OpBoolEqual True e = justAtom e
optBoolOp OpBoolEqual False e = optNot e
optBoolOp _ _ _ = Nothing

optAdd :: Int -> MAtom -> Maybe MOper
optAdd 0 e = justAtom e
optAdd a (MInt b) = justAtom (MInt (a+b))
optAdd _ _ = Nothing

optMul :: Int -> MAtom -> Maybe MOper
optMul 0 _ = justAtom (MInt 0)
optMul 1 e = justAtom e
optMul a (MInt b) = justAtom (MInt (a*b))
optMul _ _ = Nothing

optimiseArith :: MExpr -> MExpr
optimiseArith ex = optImpl >:= ex
    where
        optImpl (MLet s t mv me) = 
            let res = case mv of
                    MUnary OpNot a -> optNot a
                    MBinary OpAdd (MInt a) e -> optAdd a e
                    MBinary OpAdd e (MInt a) -> optAdd a e
                    MBinary OpSub e (MInt a) -> optAdd (-a) e
                    MBinary OpMul (MInt a) e -> optMul a e
                    MBinary OpMul e (MInt a) -> optMul a e
                    MBinary OpLessThan (MInt a) (MInt b) -> justAtom (MBool (a < b))
                    MBinary OpIntEqual (MInt a) (MInt b) -> justAtom (MBool (a == b))
                    MBinary op (MBool a) e -> optBoolOp op a e
                    MBinary op e (MBool a) -> optBoolOp op a e
                    _ -> Nothing
            in (\mv' -> MLet s t mv' me) <$> res
        optImpl _ = Nothing