module Middleend.Opt.Alpha (alphaConvert) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.Function (on)
import Data.Map qualified as M
import Id (Fresh, Id, refresh)
import Middleend.KNormal (FunBind (..), KNormal (..))

alphaConvert :: Fresh KNormal -> Fresh KNormal
alphaConvert exprM = do
  expr' <- exprM
  evalStateT (go expr') M.empty
  where
    go :: KNormal -> StateT (M.Map Id Id) Fresh KNormal
    go expr = do
      resolve <- gets (\m ident -> M.findWithDefault ident ident m)
      case expr of
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
        Let binder rhs body -> do
          binder' <- lift $ refresh binder
          modify $ M.insert binder binder'
          rhs' <- go rhs
          body' <- go body
          pure $ Let binder' rhs' body'
        Var ident -> pure . Var . resolve $ ident
        LetRec FunBind {name, args, rhs} body -> do
          name' <- lift . refresh $ name
          args' <- traverse (lift . refresh) args
          modify $ \m -> foldl' (flip $ uncurry M.insert) m $ (name, name') : zip args args'
          rhs' <- go rhs
          body' <- go body
          pure $ LetRec FunBind {name = name', args = args', rhs = rhs'} body'
        App fun params -> pure $ App (resolve fun) (resolve <$> params)
        Tuple idents -> pure . Tuple . fmap resolve $ idents
        LetTuple binders rhs body -> do
          binders' <- traverse (lift . refresh) binders
          modify $ \m -> foldl' (flip $ uncurry M.insert) m $ zip binders binders'
          body' <- go body
          pure $ LetTuple binders' (resolve rhs) body'
        Get arr idx -> pure $ (Get `on` resolve) arr idx
        Put arr idx rhs -> pure $ Put (resolve arr) (resolve idx) (resolve rhs)
