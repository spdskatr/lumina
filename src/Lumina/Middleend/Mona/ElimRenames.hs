module Lumina.Middleend.Mona.ElimRenames (elimRenames) where
import Lumina.Middleend.Mona.Mona (MAtom (..), MExpr (..), MValue (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lumina.Utils (orElse)

type RenameEnv = Map String MAtom

elimRenames :: MExpr -> MExpr
elimRenames = elimRenamesImpl Map.empty

elimRenamesImpl :: RenameEnv -> MExpr -> MExpr
elimRenamesImpl env ex = case ex of
    MLet s mv me ->
        let subMV mv' = MLet s mv' (recurse me)
        in case processVal mv of
            MJust ma ->
                -- Expression will get removed in dead code elimination
                MLet s mv (elimRenamesImpl (Map.insert s (process ma) env) me)
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
        processVal (MJust a) = MJust (process a)
        processVal (MUnary uo a) = MUnary uo (process a)
        processVal (MBinary bo a a') = MBinary bo (process a) (process a')
        processVal (MApp a a') = MApp (process a) (process a')

        recurse = elimRenamesImpl env

        process (MVar x) = env Map.!? x `orElse` MVar x
        process a = a
