module Middleend.KNormal (Exp (..), Binder (..), FunBind (..), kNormal) where

import Basic (Cmp (..), PrimOp (..))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Bool (bool)
import Data.Map qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Frontend.Syntax qualified as S
import Name (Fresh, Id, Symbol, fresh)
import Type (Type)
import Type qualified as T

-- TODO: maybe better to share with Syntax.Binder
data Binder = Binder
  { name :: Id
  , ty :: Type
  }
  deriving stock (Show, Eq)

data Exp
  = Unit
  | Int Int
  | Float Float
  | Var Id
  | PrimOp (PrimOp Id)
  | If Cmp Id Id Exp Exp
  | Let Binder Exp Exp
  | LetRec FunBind Exp
  | LetTuple [Binder] Id Exp
  | App Id [Id]
  -- \| ExtArray Id
  -- \| ExtFunApp Id [Id]
  deriving stock (Show, Eq)

data FunBind = FunBind
  { binder :: Binder
  , args :: [Binder]
  , rhs :: Exp
  }
  deriving stock (Show, Eq)

insertLet :: (Fresh :> es) => Type -> Eff es Exp -> (Id -> Eff es Exp) -> Eff es Exp
insertLet ty scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      name <- fresh "T"
      body <- k name
      pure $ Let Binder {name, ty} scrut body

kNormal :: (Fresh :> es) => S.Typed -> Eff es Exp
kNormal = runReader (M.empty :: M.Map Symbol Id) . go

go :: (Fresh :> es, Reader (M.Map Symbol Id) :> es) => S.Typed -> Eff es Exp
go expr@(_ :< t) = evalContT $ case t of
  S.Unit -> pure Unit
  S.Int int -> pure $ Int int
  S.Float float -> pure $ Float float
  S.Bool bol -> pure . Int $ bool 0 1 bol
  (S.Not {}; S.Cmp {}) -> kIf expr (pure (Int 1)) (pure (Int 0))
  S.Var sym -> Var <$> lift (asks (M.! sym))
  S.PrimOp op -> PrimOp <$> traverse atomize op
  S.If cnd thn els -> kIf cnd (go thn) (go els)
  S.Let binder rhs body -> do
    rhs' <- lift (go rhs)
    binder' <- bind binder
    Let binder' rhs' <$> lift (go body)
  S.LetRec S.FunBind {binder = binder, args, rhs} body -> do
    binder' <- bind binder
    (args', rhs') <- lift . evalContT $ do
      args' <- traverse bind args
      (args',) <$> lift (go rhs)
    LetRec FunBind {binder = binder', args = args', rhs = rhs'} <$> lift (go body)
  S.App fun params -> App <$> atomize fun <*> traverse atomize params
  S.LetTuple binders rhs body -> do
    rhs' <- atomize rhs
    binders' <- traverse bind binders
    LetTuple binders' rhs' <$> lift (go body)
  where
    atom ty = ContT . insertLet ty
    atomize e = ContT . insertLet (snd (extract e)) $ go e
    bind S.Binder {name, ty} = ContT \k -> do
      name' <- fresh name
      local (M.insert name name') (k Binder {name = name', ty})
    kIf (_ :< S.Not c) thn els = kIf c els thn
    kIf (_ :< S.Cmp cmp lhs rhs) thn els = If cmp <$> atomize lhs <*> atomize rhs <*> lift thn <*> lift els
    kIf cnd thn els = If Eq <$> atomize cnd <*> atom (T.Type T.Int) (pure (Int 1)) <*> lift thn <*> lift els
