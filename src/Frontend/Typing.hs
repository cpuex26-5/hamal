module Frontend.Typing (TypeError (..), typing) where

import Basic (Binder (..), Cmp (..), FunBind (..), PrimOp (..))
import Basic qualified as B
import Control.Comonad (Comonad (extract))
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (when, zipWithM_)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (uncons)
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Effectful.State.Static.Local (State, get, modify, runState)
import Frontend.SrcSpan (SrcSpan)
import Frontend.Syntax qualified as S
import Name (Fresh, Symbol, freshI)
import Type qualified as T

data TypeError
  = Mismatch SrcSpan Typing Typing
  | Recursive SrcSpan TyVar Typing
  | UnknownSymbol SrcSpan Symbol
  | Ambiguous SrcSpan TyVar
  | BuiltinArity SrcSpan Symbol Int Int
  deriving stock (Show, Eq)

newtype TyVar = TyVar Int
  deriving newtype (Show, Eq, Ord)

type Typing = T.TypeF TyVar

freshTy :: (Fresh :> es) => Eff es Typing
freshTy = T.Var . TyVar <$> freshI

type Subst = M.Map TyVar Typing

type Env = M.Map Symbol Typing

type Annotated = Cofree (S.ExpF Typing) (SrcSpan, Typing)

typing :: (Error TypeError :> es, Fresh :> es) => S.Parsed -> Eff es S.Typed
typing parsed = do
  (inferred, subst) <- runState @Subst M.empty . runReader @Env M.empty . infer $ parsed
  resolve subst inferred

typeof :: Annotated -> Typing
typeof = snd . extract

ascribe :: (Error TypeError :> es, State Subst :> es) => Annotated -> Typing -> Eff es ()
ascribe e expected = let (sspan, actual) = extract e in unify sspan expected actual

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

infer :: (Error TypeError :> es, Fresh :> es, State Subst :> es, Reader Env :> es) => S.Parsed -> Eff es Annotated
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
    ty <- inferPrim op'
    pure $ (sspan, ty) :< S.PrimOp op'
  S.Cmp cmp lhs rhs -> do
    lhs' <- infer lhs
    rhs' <- infer rhs
    case cmp of
      ICmp {} -> lhs' `ascribe` T.Int *> rhs' `ascribe` T.Int
      FCmp {} -> lhs' `ascribe` T.Float *> rhs' `ascribe` T.Float
    pure $ (sspan, T.Bool) :< S.Cmp cmp lhs' rhs'
  S.If cnd thn els -> do
    cnd' <- infer cnd
    thn' <- infer thn
    els' <- infer els
    cnd' `ascribe` T.Bool
    thn' `ascribe` typeof els'
    pure $ (sspan, typeof thn') :< S.If cnd' thn' els'
  S.Let Binder {name} rhs body -> do
    rhs' <- infer rhs
    let ty = typeof rhs'
    with [Binder {name, ty}] do
      body' <- infer body
      pure $ (sspan, typeof body') :< S.Let Binder {name, ty} rhs' body'
  S.LetRec FunBind {binder = Binder {name}, params, rhs} body -> do
    ret <- freshTy
    args' <- traverse annotate params
    let binder = Binder {name, ty = T.Fun ((.ty) <$> args') ret}
    rhs' <- with (binder : args') do
      rhs' <- infer rhs
      rhs' `ascribe` ret
      pure rhs'
    with [binder] do
      body' <- infer body
      pure $ (sspan, typeof body') :< S.LetRec FunBind {binder, params = args', rhs = rhs'} body'
  S.LetTuple binders rhs body -> do
    binders' <- traverse annotate binders
    rhs' <- infer rhs
    rhs' `ascribe` T.Tuple ((.ty) <$> binders')
    with binders' do
      body' <- infer body
      pure $ (sspan, typeof body') :< S.LetTuple binders' rhs' body'
  S.App fun@(_ :< S.Var sym) args -> do
    bound <- asks @Env (M.member sym)
    case lookup sym builtins of
      Just shape
        | not bound ->
            maybe
              (throwError (BuiltinArity sspan sym (length shape) (length args)))
              (infer . (sspan :<) . S.PrimOp)
              (fill shape args)
      _ -> inferApp fun args
  S.App fun args -> inferApp fun args
  where
    with binders = local (M.union $ M.fromList [(name, ty) | Binder {name, ty} <- binders])
    annotate = traverse (\() -> freshTy)
    inferPrim op = case op of
      B.Not ty -> ty `ascribe` T.Bool $> T.Bool
      B.Neg {}; B.Add {}; B.Sub {}; B.Sali {}; B.Sari {} -> traverse_ (`ascribe` T.Int) op $> T.Int
      B.FNeg {}; B.FAdd {}; B.FSub {}; B.FMul {}; B.FDiv {}; B.FSqrt {} -> traverse_ (`ascribe` T.Float) op $> T.Float
      B.Tuple tys -> pure (T.Tuple . map typeof $ tys)
      B.Array size initial -> do
        size `ascribe` T.Int
        pure $ T.Array (typeof initial)
      B.Get arr idx -> do
        t <- freshTy
        arr `ascribe` T.Array t
        idx `ascribe` T.Int
        pure t
      B.Put arr idx val -> do
        arr `ascribe` T.Array (typeof val)
        idx `ascribe` T.Int
        pure T.Unit
      B.FloatOfInt ty -> ty `ascribe` T.Int $> T.Float
      B.IntOfFloat ty -> ty `ascribe` T.Float $> T.Int
      B.Out ty -> ty `ascribe` T.Int $> T.Unit
    inferApp fun args = do
      fun' <- infer fun
      args' <- mapM infer args
      ty <- freshTy
      fun' `ascribe` T.Fun (typeof <$> args') ty
      pure $ (sspan, ty) :< S.App fun' args'
    builtins =
      [ ("not", Not ())
      , ("Array.create", Array () ())
      , ("Array.make", Array () ())
      , ("float_of_int", FloatOfInt ())
      , ("int_of_float", IntOfFloat ())
      , ("print_char", Out ())
      , ("sqrt", FSqrt ())
      ]
    fill shape xs = case runStateT (traverse (const (StateT uncons)) shape) xs of
      Just (filled, []) -> Just filled
      _ -> Nothing

zonk :: Subst -> Typing -> Typing
zonk s t = t >>= \v -> maybe (T.Var v) (zonk s) (M.lookup v s)

resolve :: forall es. (Error TypeError :> es) => Subst -> Annotated -> Eff es S.Typed
resolve subst = go
  where
    go ((sspan, ty) :< e) = ((:<) . (sspan,) <$> ground sspan ty) <*> bitraverse (ground sspan) go e
    ground sspan = fmap T.Type . traverse (throwError . Ambiguous sspan) . zonk subst
