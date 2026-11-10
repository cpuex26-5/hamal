module Middleend.Opt.TupleFlatten (flattenArgs) where

import Basic (
  Binder (..),
  PrimOp (Tuple),
 )
import Control.Monad (when, zipWithM)
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Log (Log, logMsg)
import Middleend.KNormal (Exp (..), FunBind (..))
import Name (Fresh, Id, refresh)
import Type qualified as T

data ParamPos = Keep | Spread [T.Type]

type Env = M.Map Id [ParamPos]

spread :: (Fresh :> es) => Id -> ParamPos -> Eff es (Maybe [Binder Id T.Type])
spread name = \case
  Keep -> pure Nothing
  Spread tys -> Just <$> traverse (\ty -> Binder <$> refresh name <*> pure ty) tys

flattenArgs :: (Log :> es, Fresh :> es) => Exp -> Eff es Exp
flattenArgs = runReader @Env M.empty . go
  where
    go :: (Log :> es, Fresh :> es, Reader Env :> es) => Exp -> Eff es Exp
    go = \case
      e@(Unit; Int {}; Float {}; Var {}; PrimOp {}) -> pure e
      If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      Let binder rhs body -> Let binder <$> go rhs <*> go body
      LetTuple binders rhs body -> LetTuple binders rhs <$> go body
      LetRec FunBind {binder, params, rhs} body -> do
        -- TODO: escape check
        -- TODO: limit # of params
        let ps = genPos <$> params
        ms <- zipWithM (\param -> spread param.name) params ps
        let params' = concat $ zipWith (\param -> maybe [param] id) params ms
        when (any (\case Keep -> False; Spread {} -> True) ps)
          $ logMsg
          $ "spreading " <> show binder.name <> ": " <> show params <> " -> " <> show params'
        let cod = case binder.ty of
              T.Type (T.Fun _ t) -> t
              _ -> error $ "flattenArgs(unreachable): LetRec has a type except for fun: " <> show binder.ty
        let dom = (\Binder {ty = T.Type t} -> t) <$> params'
        let funTy = T.Type (T.Fun dom cod)
        rhs' <- local (M.insert binder.name ps) (go rhs)
        body' <- local (M.insert binder.name ps) (go body)
        let prologued = foldr insertLet rhs' (zip params ms)
        pure $ LetRec FunBind {binder = binder {ty = funTy}, params = params', rhs = prologued} body'
        where
          insertLet (param, m) k = maybe k (\binders -> Let param (PrimOp $ Tuple $ (.name) <$> binders) k) m
          genPos Binder {ty} = case ty of
            T.Type (T.Tuple tys) -> Spread (T.Type <$> tys)
            _ -> Keep
      e@(App fun args) ->
        asks @Env (M.lookup fun) >>= maybe (pure e) \ps -> do
          ms <- zipWithM spread args ps
          let args' = concat $ zipWith (\arg -> maybe [arg] (fmap (.name))) args ms
          pure $
            foldr
              (\(arg, m) k -> maybe k (\binders -> LetTuple binders arg k) m)
              (App fun args')
              (zip args ms)
