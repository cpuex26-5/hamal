module Frontend.Assoc (assocLet) where

import Frontend.KNormal (FunBind (..), KNormal (..))

assocLet :: KNormal -> KNormal
assocLet = \case
  IfEq lhs rhs thn els -> IfEq lhs rhs (assocLet thn) (assocLet els)
  IfLe lhs rhs thn els -> IfLe lhs rhs (assocLet thn) (assocLet els)
  Let binder rhs body -> go (\final -> Let binder final $ assocLet body) rhs
  LetRec FunBind {..} body -> LetRec FunBind {name, args, rhs = assocLet rhs} $ assocLet body
  LetTuple binders rhs body -> LetTuple binders rhs $ assocLet body
  e -> e
  where
    go k = \case
      Let binder rhs body -> Let binder rhs $ go k body
      LetRec funbind body -> LetRec funbind $ go k body
      LetTuple binders rhs body -> LetTuple binders rhs $ go k body
      e -> k e
