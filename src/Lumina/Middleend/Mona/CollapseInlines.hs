module Lumina.Middleend.Mona.CollapseInlines (collapseInlines) where
import Lumina.Middleend.Mona.Mona (MExpr (..), (>:=), MOper (..))

collapseInlines :: MExpr -> MExpr
collapseInlines e = collapseInlinesImpl >:= e

collapseInlinesImpl :: MExpr -> Maybe MExpr
collapseInlinesImpl e = case e of
    MLetInline x t me me' -> 
        let rest = collapseInlinesImpl >:= me'
        in case collapseInlinesImpl >:= me of
            MLet y t' v nx -> Just $ MLet y t' v (MLetInline x t nx rest)
            MAssign a b nx -> Just $ MAssign a b (MLetInline x t nx rest)
            MIf ma th el -> Just $ MLetInline x t (MIf ma th el) rest
            MLetInline y t2 me2 nx -> Just $ MLetInline y t2 me2 (MLetInline x t nx rest)
            MReturn ma -> Just $ MLet x t (MJust ma) rest
    _ -> Nothing
