module Middleend.Opt.Alpha (alphaConvert) where

import Data.Function (on)
import Data.Map qualified as M
import Effectful ((:>))
import Effectful.Internal.Monad (Eff)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Id (Fresh, Id, refresh)
import Middleend.KNormal (Exp (..), FunBind (..))

alphaConvert :: (Fresh :> es) => Exp -> Eff es Exp
alphaConvert = runReader @(M.Map Id Id) M.empty . go
  where
    go :: (Fresh :> es, Reader (M.Map Id Id) :> es) => Exp -> Eff es Exp
    go expr = do
      resolve <- asks (\m ident -> M.findWithDefault ident ident m)
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
          binder' <- refresh binder
          rhs' <- go rhs
          body' <- local (M.insert binder binder') $ go body
          pure $ Let binder' rhs' body'
        Var ident -> pure . Var . resolve $ ident
        LetRec FunBind {name, args, rhs} body -> do
          name' <- refresh name
          args' <- traverse refresh args
          rhs' <- local (\m -> foldl' (flip $ uncurry M.insert) m $ zip args args') $ go rhs
          body' <- local (M.insert name name') $ go body
          pure $ LetRec FunBind {name = name', args = args', rhs = rhs'} body'
        App fun params -> pure $ App (resolve fun) (resolve <$> params)
        Tuple idents -> pure . Tuple . fmap resolve $ idents
        LetTuple binders rhs body -> do
          binders' <- traverse refresh binders
          body' <- local (\m -> foldl' (flip $ uncurry M.insert) m $ zip binders binders') $ go body
          pure $ LetTuple binders' (resolve rhs) body'
        Get arr idx -> pure $ (Get `on` resolve) arr idx
        Put arr idx rhs -> pure $ Put (resolve arr) (resolve idx) (resolve rhs)
