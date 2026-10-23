module Middleend.Opt.Beta (betaConvert) where

import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Function (on)
import Data.Map qualified as M
import Id (Id)
import Middleend.KNormal (Exp (..), FunBind (..))

betaConvert :: Exp -> Exp
betaConvert expr = evalState (go expr) M.empty
  where
    go :: Exp -> State (M.Map Id Id) Exp
    go expr' = do
      resolve <- gets (\m ident -> M.findWithDefault ident ident m)
      case expr' of
        e@(Unit; Int _; Float _) -> pure e
        Neg e -> pure . Neg . resolve $ e
        FNeg e -> pure . FNeg . resolve $ e
        Add lhs rhs -> pure $ (Add `on` resolve) lhs rhs
        Sub lhs rhs -> pure $ (Sub `on` resolve) lhs rhs
        FAdd lhs rhs -> pure $ (FAdd `on` resolve) lhs rhs
        FSub lhs rhs -> pure $ (FSub `on` resolve) lhs rhs
        FMul lhs rhs -> pure $ (FMul `on` resolve) lhs rhs
        FDiv lhs rhs -> pure $ (FDiv `on` resolve) lhs rhs
        IfEq lhs rhs thn els -> (IfEq `on` resolve) lhs rhs <$> go thn <*> go els
        IfLe lhs rhs thn els -> (IfLe `on` resolve) lhs rhs <$> go thn <*> go els
        Let binder rhs body -> case rhs of
          -- TODO: report beta-reduction
          Var bound -> modify (M.insert bound binder) >> go body
          _ -> Let binder rhs <$> go body
        Var ident -> pure . Var . resolve $ ident
        LetRec FunBind {name, args, rhs} body ->
          (\rhs' -> LetRec FunBind {name, args, rhs = rhs'}) <$> go rhs <*> go body
        App fun params -> pure $ App (resolve fun) (resolve <$> params)
        Tuple idents -> pure . Tuple . fmap resolve $ idents
        LetTuple binders rhs body -> LetTuple binders rhs <$> go body
        Get arr idx -> pure $ (Get `on` resolve) arr idx
        Put arr idx rhs -> pure $ Put (resolve arr) (resolve idx) (resolve rhs)
