module Lumina.Middleend.Mona.CollapseInlines (collapseInlines) where
import Lumina.Middleend.Mona.Mona (MExpr (..), (>:=), MOper (..))
import Lumina.Middleend.Typing (LuminaType)

collapseInlines :: MExpr -> MExpr
collapseInlines e = collapseInlinesImpl >:= e

collapseInlinesImpl :: MExpr -> Maybe MExpr
collapseInlinesImpl e = case e of
    MLetInline x t me me' -> 
        Just $ contInline x t me me'
    _ -> Nothing

contInline :: String -> LuminaType -> MExpr -> MExpr -> MExpr
contInline x t inner rest = case inner of
    MLet y t2 v nx -> MLet y t2 v (contInline x t nx rest)
    MAssign a b nx -> MAssign a b (contInline x t nx rest)
    MLetInline y t2 inner2 rest2 -> contInline y t2 inner2 (contInline x t rest2 rest)
    -- Stop if about to collapse a branching path or reached a return statement
    MIf ma th el -> MLetInline x t (MIf ma th el) (collapseInlinesImpl >:= rest)
    MReturn ma -> MLet x t (MJust ma) (collapseInlinesImpl >:= rest)
