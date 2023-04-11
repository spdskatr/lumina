module Lumina.Middleend.Mona.PropagateConsts (propagateConsts) where
import Lumina.Middleend.Mona.Mona (MAtom (..), MExpr (..), MOper (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (orElse)
import Lumina.Middleend.Typing (LuminaType(..))

type RenameEnv = Map String MAtom

propagateConsts :: MExpr -> MExpr
propagateConsts = propagateConstsImpl Map.empty

propagateConstsImpl :: RenameEnv -> MExpr -> MExpr
propagateConstsImpl env ex = case ex of
    MLet s TUnit mv me ->
        MLet s TUnit mv (propagateConstsImpl (Map.insert s MUnit env) me)
    MLet s t mv me ->
        let subMV mv' = MLet s t mv' (recurse me)
        in case mv of
            MJust ma ->
                -- Expression will get removed in dead code elimination
                MLet s t mv (propagateConstsImpl (Map.insert s (process ma) env) me)
            MMkClosure c vs ->
                subMV (MMkClosure c [process ma | ma <- vs])
            MUnary uo ma ->
                subMV (MUnary uo (process ma))
            MBinary bo ma ma' ->
                subMV (MBinary bo (process ma) (process ma'))
            MApp ma ma' ->
                subMV (MApp (process ma) (process ma'))
    MIf ma me me' ->
        MIf (process ma) (recurse me) (recurse me')
    MReturn ma -> MReturn (process ma)
    MAssign ma ma' me ->
        MAssign (process ma) (process ma') (recurse me)
    where
        recurse = propagateConstsImpl env

        process (MVar x) = env Map.!? x `orElse` MVar x
        process a = a
