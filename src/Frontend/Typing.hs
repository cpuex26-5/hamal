module Frontend.Typing (TypeError (..), typing) where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree (..), hoistCofree)
import Control.Monad (when, zipWithM_)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Map qualified as M
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Effectful.State.Static.Local (State, get, modify, runState)
import Frontend.SrcSpan (SrcSpan)
import Frontend.Syntax qualified as S
import Name (Fresh, Symbol, freshI)
import PrimOp qualified as P
import Type qualified as T

data TypeError
  = Mismatch SrcSpan Typing Typing
  | Recursive SrcSpan TyVar Typing
  | UnknownSymbol SrcSpan Symbol
  deriving stock (Show, Eq)

newtype TyVar = TyVar Int
  deriving newtype (Show, Eq, Ord)

type Typing = T.TypeF TyVar

freshTy :: (Fresh :> es) => Eff es Typing
freshTy = T.Var . TyVar <$> freshI

type Subst = M.Map TyVar Typing

type Env = M.Map Symbol Typing

typing :: (Error TypeError :> es, Fresh :> es) => S.Parsed -> Eff es S.Typed
typing parsed = do
  (inferred, subst) <- runState @Subst M.empty . runReader @Env M.empty . infer $ parsed
  pure $ resolve subst inferred

infer :: (Error TypeError :> es, Fresh :> es, State Subst :> es, Reader Env :> es) => S.Parsed -> Eff es (Cofree (S.ExpF Typing) (SrcSpan, Typing))
infer (sspan :< expr) = case expr of
  S.Unit -> pure $ (sspan, T.Unit) :< S.Unit
  S.Bool bool -> pure $ (sspan, T.Bool) :< S.Bool bool
  S.Int int -> pure $ (sspan, T.Int) :< S.Int int
  S.Float float -> pure $ (sspan, T.Float) :< S.Float float
  S.Var sym -> do
    ty <- maybe (throwError (UnknownSymbol sspan sym)) pure =<< asks (M.lookup sym)
    pure $ (sspan, ty) :< S.Var sym
  S.PrimOp op -> do
    op' <- traverse infer op
    ty <- inferPrim (typeOf <$> op')
    pure $ (sspan, ty) :< S.PrimOp op'
  S.Not e -> do
    e' <- infer e
    unify sspan T.Bool (typeOf e')
    pure $ (sspan, T.Bool) :< S.Not e'
  S.Cmp cmp lhs rhs -> do
    lhs' <- infer lhs
    rhs' <- infer rhs
    -- TODO: limit to int/float
    typeOf lhs' =!= typeOf rhs'
    pure $ (sspan, T.Bool) :< S.Cmp cmp lhs' rhs'
  S.If cnd thn els -> do
    cnd' <- infer cnd
    thn' <- infer thn
    els' <- infer els
    typeOf cnd' =!= T.Bool
    typeOf thn' =!= typeOf els'
    pure $ (sspan, typeOf thn') :< S.If cnd' thn' els'
  S.Let S.Binder {name} rhs body -> do
    rhs' <- infer rhs
    let ty = typeOf rhs'
    withBinders [S.Binder {name, ty}] do
      body' <- infer body
      pure $ (sspan, typeOf body') :< S.Let S.Binder {name, ty} rhs' body'
  S.LetRec S.FunBind {binder = S.Binder {name}, args, rhs} body -> do
    ty <- freshTy
    args' <- traverse annotate args
    let binder = S.Binder {name, ty}
    withBinders (binder : args') do
      rhs' <- infer rhs
      body' <- infer body
      ty =!= T.Fun ((.ty) <$> args') (typeOf rhs')
      pure $ (sspan, typeOf body') :< S.LetRec S.FunBind {binder, args = args', rhs = rhs'} body'
  S.LetTuple binders rhs body -> do
    binders' <- traverse annotate binders
    rhs' <- infer rhs
    typeOf rhs' =!= T.Tuple ((.ty) <$> binders')
    withBinders binders' do
      body' <- infer body
      pure $ (sspan, typeOf body') :< S.LetTuple binders' rhs' body'
  S.App fun params -> do
    fun' <- infer fun
    params' <- mapM infer params
    ty <- freshTy
    typeOf fun' =!= T.Fun (typeOf <$> params') ty
    pure $ (sspan, ty) :< S.App fun' params'
  S.Array size initial -> do
    size' <- infer size
    initial' <- infer initial
    typeOf size' =!= T.Int
    pure $ (sspan, T.Array (typeOf initial')) :< S.Array size' initial'
  where
    actual =!= expected = unify sspan expected actual
    typeOf = snd . extract
    withBinders binders = local (M.union $ M.fromList [(name, ty) | S.Binder {name, ty} <- binders])
    annotate = traverse (\() -> freshTy)
    inferPrim op = case op of
      P.Neg {}; P.Add {}; P.Sub {}; P.Mul {}; P.Div {} -> traverse (=!= T.Int) op $> T.Int
      P.FNeg {}; P.FAdd {}; P.FSub {}; P.FMul {}; P.FDiv {} -> traverse (=!= T.Float) op $> T.Float
      P.Tuple tys -> pure (T.Tuple tys)
      P.Get arr idx -> do
        t <- freshTy
        arr =!= T.Array t
        idx =!= T.Int
        pure t
      P.Put arr idx val -> do
        arr =!= T.Array val
        idx =!= T.Int
        pure T.Unit

-- TODO: stop to fallback to int
zonk :: Subst -> Typing -> T.TypeF a
zonk s t = t >>= \v -> maybe T.Int (zonk s) (M.lookup v s)

unify :: (Error TypeError :> es, State Subst :> es) => SrcSpan -> Typing -> Typing -> Eff es ()
unify sspan expected actual = do
  subst <- get
  let expected' = zonk subst expected
      actual' = zonk subst actual
  case (expected', actual') of
    _ | expected' == actual' -> pure ()
    (T.Var v, t) -> bind v t
    (t, T.Var v) -> bind v t
    (T.Tuple tys, T.Tuple tys') | length tys == length tys' -> zipWithM_ (unify sspan) tys tys'
    (T.Fun args rhs, T.Fun args' rhs') | length args == length args' -> zipWithM_ (unify sspan) (rhs : args) (rhs' : args')
    (T.Array t, T.Array t') -> unify sspan t t'
    _ -> throwError (Mismatch sspan expected' actual')
  where
    bind v t = when (v `elem` t) (throwError (Recursive sspan v t)) *> modify (M.insert v t)

resolve :: Subst -> Cofree (S.ExpF Typing) (SrcSpan, Typing) -> S.Typed
resolve subst = hoistCofree (first ground) . fmap (ground . snd)
  where
    ground = T.Type . zonk subst
