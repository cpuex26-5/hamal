module Middleend.Opt.Beta (betaConvert) where

import Data.Function (on)
import Data.Map qualified as M
import Id (Id)
import Middleend.KNormal (Exp (..), FunBind (..))

betaConvert :: Exp -> Exp
betaConvert = go M.empty
  where
    go :: M.Map Id Id -> Exp -> Exp
    go env = \case
      e@(Unit; Int _; Float _) -> e
      Neg e -> Neg . resolve $ e
      FNeg e -> FNeg . resolve $ e
      Add lhs rhs -> (Add `on` resolve) lhs rhs
      Sub lhs rhs -> (Sub `on` resolve) lhs rhs
      FAdd lhs rhs -> (FAdd `on` resolve) lhs rhs
      FSub lhs rhs -> (FSub `on` resolve) lhs rhs
      FMul lhs rhs -> (FMul `on` resolve) lhs rhs
      FDiv lhs rhs -> (FDiv `on` resolve) lhs rhs
      IfEq lhs rhs thn els -> (IfEq `on` resolve) lhs rhs (go env thn) (go env els)
      IfLe lhs rhs thn els -> (IfLe `on` resolve) lhs rhs (go env thn) (go env els)
      Let binder rhs body -> case rhs of
        -- TODO: report beta-reduction
        Var bound -> go (M.insert binder bound env) body
        _ -> Let binder rhs (go env body)
      Var ident -> Var . resolve $ ident
      LetRec FunBind {name, args, rhs} body -> LetRec FunBind {name, args, rhs = go env rhs} (go env body)
      App fun params -> App (resolve fun) (resolve <$> params)
      Tuple idents -> Tuple (resolve <$> idents)
      LetTuple binders rhs body -> LetTuple binders rhs (go env body)
      Get arr idx -> (Get `on` resolve) arr idx
      Put arr idx rhs -> Put (resolve arr) (resolve idx) (resolve rhs)
      where
        resolve ident = M.findWithDefault ident ident env
