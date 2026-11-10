module Middleend.Opt.TupleFlatten (flattenArgs, flattenLetTuple) where

import Basic (
  Binder (..),
  PrimOp (Tuple),
 )
import Control.Monad (when, zipWithM)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Log (Log, logMsg)
import Middleend.KNormal (Exp (..), FunBind (..))
import Name (Fresh, Id, refresh)
import Type qualified as T

unknownFun :: Exp -> S.Set Id
unknownFun = go S.empty
  where
    go :: S.Set Id -> Exp -> S.Set Id
    go env = \case
      (Unit; Int {}; Float {}; Var {}; PrimOp {}) -> S.empty
      If _ _ _ cnd thn -> go env cnd <> go env thn
      Let _ rhs body -> go env rhs <> go env body
      LetRec funbind body -> go (S.insert funbind.binder.name env) body
      LetTuple _ _ body -> go env body
      App _ args -> S.fromList $ filter (\arg -> S.member arg env) args

data ParamPos = Keep | Spread [T.Type]

type Env = M.Map Id [ParamPos]

spread :: (Fresh :> es) => Id -> ParamPos -> Eff es (Maybe [Binder Id T.Type])
spread name = \case
  Keep -> pure Nothing
  Spread tys -> Just <$> traverse (\ty -> Binder <$> refresh name <*> pure ty) tys

flattenArgs :: (Log :> es, Fresh :> es) => Exp -> Eff es Exp
flattenArgs expr = runReader @Env M.empty (go expr)
  where
    unknowns = unknownFun expr
    go :: (Log :> es, Fresh :> es, Reader Env :> es) => Exp -> Eff es Exp
    go = \case
      e@(Unit; Int {}; Float {}; Var {}; PrimOp {}) -> pure e
      If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      Let binder rhs body -> Let binder <$> go rhs <*> go body
      LetTuple binders rhs body -> LetTuple binders rhs <$> go body
      LetRec fb body | (fb.binder.name `S.member` unknowns) -> LetRec fb <$> go body
      LetRec FunBind {binder, params, rhs} body -> do
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

flattenLetTuple :: Exp -> Exp
flattenLetTuple = go M.empty
  where
    go env = \case
      e@(Unit; Int {}; Float {}; Var {}; PrimOp {}; App {}) -> e
      If cmp lhs rhs thn els -> If cmp lhs rhs (go env thn) (go env els)
      LetRec FunBind {binder, params, rhs} body -> LetRec FunBind {binder, params, rhs = go env rhs} (go env body)
      Let binder rhs@(PrimOp (Tuple idents)) body -> Let binder rhs $ go (M.insert binder.name idents env) body
      Let binder rhs body -> Let binder rhs (go env body)
      LetTuple binders rhs body
        | Just untupling <- M.lookup rhs env ->
            foldr
              (\(binder, ident) k -> Let binder (Var ident) k)
              (go env body)
              (zip binders untupling)
      LetTuple binders rhs body -> LetTuple binders rhs (go env body)
