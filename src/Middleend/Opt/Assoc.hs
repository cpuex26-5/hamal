module Middleend.Opt.Assoc (assocLet) where

import Middleend.KNormal (Exp (..))

assocLet :: Exp -> Exp
assocLet = \case
  e@(Unit {}; Int {}; Float {}; Var {}; PrimOp {}; App {}) -> e
  If cmp lhs rhs thn els -> If cmp lhs rhs (assocLet thn) (assocLet els)
  Let binder rhs body -> go (\final -> Let binder final $ assocLet body) rhs
  LetRec funbind body -> LetRec (assocLet <$> funbind) $ assocLet body
  LetTuple binders rhs body -> LetTuple binders rhs $ assocLet body
  where
    go k = \case
      e@(Unit {}; Int {}; Float {}; Var {}; PrimOp {}; If {}; App {}) -> k e
      Let binder rhs body -> Let binder (assocLet rhs) $ go k body
      LetRec funbind body -> LetRec funbind $ go k body
      LetTuple binders rhs body -> LetTuple binders rhs $ go k body
