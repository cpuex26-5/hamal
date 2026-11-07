module Middleend.Opt.Beta (betaConvert) where

import Data.Map qualified as M
import Effectful (Eff, (:>))
import Log (Log, logMsg)
import Middleend.KNormal (Binder (..), Exp (..))
import Name (Id)

betaConvert :: forall es. (Log :> es) => Exp -> Eff es Exp
betaConvert = go M.empty
  where
    go :: M.Map Id Id -> Exp -> Eff es Exp
    go env = \case
      e@(Unit; Int _; Float _) -> pure e
      PrimOp op -> pure $ PrimOp (resolve <$> op)
      If cmp lhs rhs thn els -> If cmp (resolve lhs) (resolve rhs) <$> go env thn <*> go env els
      Let binder rhs body -> case rhs of
        Var bound -> do
          logMsg ("beta reduction: " <> show bound <> " -> " <> show binder.name)
          go (M.insert bound binder.name env) body
        _ -> Let binder rhs <$> go env body
      Var ident -> pure . Var . resolve $ ident
      LetRec funbind body -> LetRec <$> traverse (go env) funbind <*> go env body
      App fun params -> pure $ App (resolve fun) (resolve <$> params)
      LetTuple binders rhs body -> LetTuple binders rhs <$> go env body
      where
        resolve ident = M.findWithDefault ident ident env
