module Lumina.Middleend.Shortcuts (transform) where
import Lumina.Middleend.EliminateEtaRedex (fullyElimEta)
import Lumina.Middleend.CPSConvert (toCPS)
import Lumina.Frontend.LuminaAST (AST)

transform :: AST -> AST
transform = fullyElimEta . toCPS