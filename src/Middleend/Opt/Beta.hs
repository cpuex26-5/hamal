module Middleend.Opt.Beta (betaConvert) where

import Data.Map qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Log (Log, logMsg)
import Middleend.KNormal (Binder (..), Exp (..))
import Name (Id)

type Env = M.Map Id Id

betaConvert :: (Log :> es) => Exp -> Eff es Exp
betaConvert = runReader (M.empty :: Env) . go
  where
    go :: (Log :> es, Reader Env :> es) => Exp -> Eff es Exp
    go = \case
      e@(Unit; Int _; Float _) -> pure e
      PrimOp op -> PrimOp <$> traverse beta op
      If cmp lhs rhs thn els -> If cmp <$> beta lhs <*> beta rhs <*> go thn <*> go els
      Let binder rhs body ->
        go rhs >>= \case
          Var bound -> do
            logMsg ("beta reduction: " <> show binder.name <> " -> " <> show bound)
            local (M.insert binder.name bound) (go body)
          rhs' -> Let binder rhs' <$> go body
      Var ident -> Var <$> beta ident
      LetRec funbind body -> LetRec <$> traverse go funbind <*> go body
      App fun params -> App <$> beta fun <*> traverse beta params
      LetTuple binders rhs body -> LetTuple binders <$> beta rhs <*> go body
      where
        beta ident = asks (M.findWithDefault ident ident)
