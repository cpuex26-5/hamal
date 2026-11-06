module Middleend.Opt.Beta (betaConvert) where

import Data.Map qualified as M
import Middleend.KNormal (Binder (..), Exp (..), FunBind (..))
import Name (Id)

betaConvert :: Exp -> Exp
betaConvert = go M.empty
  where
    go :: M.Map Id Id -> Exp -> Exp
    go env = \case
      e@(Unit; Int _; Float _) -> e
      PrimOp op -> PrimOp (resolve <$> op)
      If cmp lhs rhs thn els -> If cmp (resolve lhs) (resolve rhs) (go env thn) (go env els)
      Let binder rhs body -> case rhs of
        -- TODO: report beta-reduction
        Var bound -> go (M.insert binder.name bound env) body
        _ -> Let binder rhs (go env body)
      Var ident -> Var . resolve $ ident
      LetRec funbind body -> LetRec funbind {rhs = go env funbind.rhs} (go env body)
      App fun params -> App (resolve fun) (resolve <$> params)
      LetTuple binders rhs body -> LetTuple binders rhs (go env body)
      where
        resolve ident = M.findWithDefault ident ident env
