module Frontend.KNormal (KNormal (..), go, kNormal) where

import Control.Monad.Trans.State (State, runState)
import Data.Bool (bool)
import Frontend.Id (Counter, Id (..), initialCounter, tmpId)
import Frontend.Syntax (Syntax (..))
import Frontend.Syntax qualified as S

data KNormal
  = Unit
  | Int Int
  | Float Float
  | Neg Id
  | Add Id Id
  | Sub Id Id
  | FNeg Id
  | FAdd Id Id
  | FSub Id Id
  | FMul Id Id
  | FDiv Id Id
  | IfEq Id Id KNormal KNormal
  | IfLe Id Id KNormal KNormal
  | Let Id KNormal KNormal
  | Var Id
  | LetRec FunBind KNormal
  | App Id [Id]
  | Tuple [Id]
  | LetTuple [Id] Id KNormal
  | Get Id Id
  | Put Id Id Id
  | -- | ExtArray Id
    ExtFunApp Id [Id]
  deriving stock (Show, Eq)

data FunBind = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: KNormal
  }
  deriving stock (Show, Eq)

type M = State Counter

insertLet :: M KNormal -> (Id -> M KNormal) -> M KNormal
insertLet scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      fresh <- tmpId
      body <- k fresh
      pure $ Let fresh scrut body

kNormal :: Syntax a -> KNormal
kNormal expr = fst $ runState (go expr) initialCounter

go :: Syntax a -> M KNormal
go Syntax {ann, kind} = case kind of
  S.Unit -> pure Unit
  S.Int int -> pure $ Int int
  S.Float float -> pure $ Float float
  S.Bool bol -> pure . Int $ bool 0 1 bol
  S.Not expr -> go $ Syntax ann $ S.If expr (Syntax ann (S.Bool False)) (Syntax ann (S.Bool True))
  S.Neg expr -> insertLet (go expr) (pure . Neg)
  S.FNeg expr -> insertLet (go expr) (pure . FNeg)
  S.Add lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . Add l)
  S.Sub lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . Sub l)
  S.FAdd lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . FAdd l)
  S.FSub lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . FSub l)
  S.FMul lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . FMul l)
  S.FDiv lhs rhs -> insertLet (go lhs) \l -> insertLet (go rhs) (pure . FDiv l)
  (S.Eq _ _; S.Le _ _) ->
    go . Syntax ann $
      S.If Syntax {ann, kind} (Syntax ann (S.Bool True)) (Syntax ann (S.Bool False))
  S.If Syntax {kind = S.Eq lhs rhs} thn els ->
    insertLet (go lhs) \l ->
      insertLet (go rhs) \r -> do
        thn' <- go thn
        els' <- go els
        pure $ IfEq l r thn' els'
  S.If Syntax {kind = S.Le lhs rhs} thn els ->
    insertLet (go lhs) \l ->
      insertLet (go rhs) \r -> do
        thn' <- go thn
        els' <- go els
        pure $ IfLe l r thn' els'
  S.If cnd thn els ->
    go . Syntax ann $
      S.If (Syntax cnd.ann $ S.Eq cnd (Syntax ann (S.Bool True))) thn els
  S.Let binder rhs body -> do
    rhs' <- go rhs
    body' <- go body
    pure $ Let binder rhs' body'
  S.Var name -> pure $ Var name
  S.LetRec S.FunBind {name, args, rhs} body -> do
    rhs' <- go rhs
    body' <- go body
    pure $ LetRec (FunBind {name, args, rhs = rhs'}) body'
  -- TODO: external functions
  S.App fun params ->
    insertLet (go fun) \fun' ->
      bind params (App fun') []
  S.Tuple exprs -> bind exprs Tuple []
  S.LetTuple binders rhs body -> insertLet (go rhs) \rhs' -> do
    body' <- go body
    pure $ LetTuple binders rhs' body'
  -- TODO: create_float_array
  S.Array len initial -> insertLet (go len) \len' ->
    insertLet (go initial) \initial' ->
      pure $ ExtFunApp (Id "create_array") [len', initial']
  S.Get lhs idx -> insertLet (go lhs) \lhs' ->
    insertLet (go idx) \idx' ->
      pure $ Get lhs' idx'
  S.Put lhs idx rhs -> insertLet (go lhs) \lhs' ->
    insertLet (go idx) \idx' ->
      insertLet (go rhs) \rhs' ->
        pure $ Put lhs' idx' rhs'
  where
    bind exprs constr =
      foldl'
        (\k m args -> insertLet m $ \arg -> k $ args <> [arg])
        (pure . constr)
        (go <$> exprs)
