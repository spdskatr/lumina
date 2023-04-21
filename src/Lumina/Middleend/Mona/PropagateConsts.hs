module Lumina.Middleend.Mona.PropagateConsts (propagateConsts) where
import Lumina.Middleend.Mona.Mona (MAtom (..), MExpr (..), MOper (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (orElse)
import Lumina.Middleend.Typing (LuminaType(..))

type RenameEnv = Map String MAtom
type ClosurePropEnv = Map String (String, [(String, MAtom)])

propagateConsts :: MExpr -> MExpr
propagateConsts = propagateConstsImpl Map.empty Map.empty

propagateConstsImpl :: ClosurePropEnv -> RenameEnv -> MExpr -> MExpr
propagateConstsImpl cenv env ex = case ex of
    MLet s TUnit mv me ->
        MLet s TUnit mv (propagateConstsImpl cenv (Map.insert s MUnit env) me)
    MLet s t mv me ->
        let subMV mv' = MLet s t mv' (recurse me)
        in case mv of
            MJust ma ->
                -- Expression will get removed in dead code elimination
                MLet s t mv (propagateConstsImpl cenv (Map.insert s (process ma) env) me)
            MMkClosure c vs ->
                MLet s t (MMkClosure c [(x, process ma) | (x, ma) <- vs]) (propagateConstsImpl (Map.insert s (c, vs) cenv) env me)
            MUnary uo ma ->
                subMV (MUnary uo (process ma))
            MBinary bo ma ma' ->
                subMV (MBinary bo (process ma) (process ma'))
            MApp ma ma' -> case processCall $ process ma of
                -- Convert immediately invoked closure application into direct calls
                Right (c,vs) -> subMV (MCall c vs (process ma'))
                Left cl -> subMV (MApp cl (process ma'))
            MCall f vs ma ->
                subMV (MCall f [(x, process a) | (x, a) <- vs] (process ma))
    MLetInline s t mv me ->
        MLetInline s t (recurse mv) (recurse me)
    MIf ma me me' ->
        MIf (process ma) (recurse me) (recurse me')
    MReturn ma -> MReturn (process ma)
    MAssign ma ma' me ->
        MAssign (process ma) (process ma') (recurse me)
    where
        recurse = propagateConstsImpl cenv env

        process (MVar x) = env Map.!? x `orElse` MVar x
        process a = a

        processCall (MVar x) = (Right <$> cenv Map.!? x) `orElse` Left (MVar x)
        processCall a = Left a
