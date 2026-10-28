module Middleend.KNormal (Exp (..), FunBind (..), kNormal) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Bool (bool)
import Effectful (Eff, (:>))
import Frontend.Syntax (Syntax (..))
import Frontend.Syntax qualified as S
import Name (Fresh, Id, Symbol (..), fresh)
import PrimOp (Cmp (..), PrimOp (..))

data Exp
  = Unit
  | Int Int
  | Float Float
  | Var Id
  | PrimOp (PrimOp Id)
  | If Cmp Id Id Exp Exp
  | Let Id Exp Exp
  | LetRec FunBind Exp
  | LetTuple [Id] Id Exp
  | App Id [Id]
  -- \| ExtArray Id
  -- \| ExtFunApp Id [Id]
  deriving stock (Show, Eq)

data FunBind = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: Exp
  }
  deriving stock (Show, Eq)

insertLet :: (Fresh :> es) => Eff es Exp -> (Id -> Eff es Exp) -> Eff es Exp
insertLet scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      tmp <- fresh "T"
      body <- k tmp
      pure $ Let tmp scrut body

kNormal :: (Fresh :> es) => Syntax a -> Eff es Exp
kNormal s = evalContT $ case s.kind of
  S.Unit -> pure Unit
  S.Int int -> pure $ Int int
  S.Float float -> pure $ Float float
  S.Bool bol -> pure . Int $ bool 0 1 bol
  (S.Not {}; S.Cmp {}) -> kIf s (pure (Int 1)) (pure (Int 0))
  S.Var (Symbol sym) -> Var <$> lift (fresh sym)
  S.PrimOp op -> PrimOp <$> traverse atomize op
  S.If cnd thn els -> kIf cnd (kNormal thn) (kNormal els)
  S.Let binder rhs body -> Let <$> (\(Symbol sym) -> lift (fresh sym)) binder <*> lift (kNormal rhs) <*> lift (kNormal body)
  S.LetRec S.FunBind {name = Symbol name, args, rhs} body -> do
    name' <- lift (fresh name)
    args' <- freshes args
    rhs' <- lift $ kNormal rhs
    body' <- lift $ kNormal body
    pure $ LetRec (FunBind {name = name', args = args', rhs = rhs'}) body'
  -- TODO: external functions
  S.App fun params -> App <$> atomize fun <*> traverse atomize params
  S.LetTuple binders rhs body -> LetTuple <$> freshes binders <*> atomize rhs <*> lift (kNormal body)
  -- TODO: create_float_array
  -- TODO: PrimOp
  S.Array len initial -> App <$> lift (fresh "create_array") <*> traverse atomize [len, initial]
  where
    freshes = mapM (\(Symbol sym) -> lift (fresh sym))
    atom = ContT . insertLet
    atomize = atom . kNormal
    kIf cnd thn els = case cnd.kind of
      S.Not c -> kIf c els thn
      S.Cmp cmp lhs rhs -> If cmp <$> atomize lhs <*> atomize rhs <*> lift thn <*> lift els
      _ -> If Eq <$> atomize cnd <*> atom (pure (Int 1)) <*> lift thn <*> lift els
