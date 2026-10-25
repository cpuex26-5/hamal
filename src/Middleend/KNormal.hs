module Middleend.KNormal (Exp (..), FunBind (..), kNormal) where

import Data.Bool (bool)
import Frontend.Syntax (Syntax (..))
import Frontend.Syntax qualified as S
import Id (Fresh, Id, fresh, mkName)

data Exp
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
  | IfEq Id Id Exp Exp
  | IfLe Id Id Exp Exp
  | Let Id Exp Exp
  | Var Id
  | LetRec FunBind Exp
  | App Id [Id]
  | Tuple [Id]
  | LetTuple [Id] Id Exp
  | Get Id Id
  | Put Id Id Id
  -- \| ExtArray Id
  -- \| ExtFunApp Id [Id]
  deriving stock (Show, Eq)

data FunBind = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: Exp
  }
  deriving stock (Show, Eq)

insertLet :: Fresh Exp -> (Id -> Fresh Exp) -> Fresh Exp
insertLet scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      tmp <- fresh "T"
      body <- k tmp
      pure $ Let tmp scrut body

kNormal :: Syntax a -> Fresh Exp
kNormal Syntax {ann, kind} = case kind of
  S.Unit -> pure Unit
  S.Int int -> pure $ Int int
  S.Float float -> pure $ Float float
  S.Bool bol -> pure . Int $ bool 0 1 bol
  S.Not e -> kNormal $ Syntax ann $ S.If e (Syntax ann (S.Bool False)) (Syntax ann (S.Bool True))
  S.Neg e -> insertLet (kNormal e) (pure . Neg)
  S.FNeg e -> insertLet (kNormal e) (pure . FNeg)
  S.Add lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . Add l)
  S.Sub lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . Sub l)
  S.FAdd lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . FAdd l)
  S.FSub lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . FSub l)
  S.FMul lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . FMul l)
  S.FDiv lhs rhs -> insertLet (kNormal lhs) \l -> insertLet (kNormal rhs) (pure . FDiv l)
  (S.Eq _ _; S.Le _ _) ->
    kNormal . Syntax ann $
      S.If Syntax {ann, kind} (Syntax ann (S.Bool True)) (Syntax ann (S.Bool False))
  S.If Syntax {kind = S.Eq lhs rhs} thn els ->
    insertLet (kNormal lhs) \l ->
      insertLet (kNormal rhs) \r -> do
        thn' <- kNormal thn
        els' <- kNormal els
        pure $ IfEq l r thn' els'
  S.If Syntax {kind = S.Le lhs rhs} thn els ->
    insertLet (kNormal lhs) \l ->
      insertLet (kNormal rhs) \r -> do
        thn' <- kNormal thn
        els' <- kNormal els
        pure $ IfLe l r thn' els'
  S.If cnd thn els ->
    kNormal . Syntax ann $
      S.If (Syntax cnd.ann $ S.Eq cnd (Syntax ann (S.Bool True))) thn els
  S.Let binder rhs body -> do
    rhs' <- kNormal rhs
    body' <- kNormal body
    pure $ Let binder rhs' body'
  S.Var name -> pure $ Var name
  S.LetRec S.FunBind {name, args, rhs} body -> do
    rhs' <- kNormal rhs
    body' <- kNormal body
    pure $ LetRec (FunBind {name, args, rhs = rhs'}) body'
  -- TODO: external functions
  S.App fun params ->
    insertLet (kNormal fun) \fun' ->
      bind params (App fun') []
  S.Tuple exprs -> bind exprs Tuple []
  S.LetTuple binders rhs body -> insertLet (kNormal rhs) \rhs' -> do
    body' <- kNormal body
    pure $ LetTuple binders rhs' body'
  -- TODO: create_float_array
  S.Array len initial -> insertLet (kNormal len) \len' ->
    insertLet (kNormal initial) \initial' ->
      pure $ App (mkName "create_array") [len', initial']
  S.Get arr idx -> insertLet (kNormal arr) \lhs' ->
    insertLet (kNormal idx) (pure . Get lhs')
  S.Put arr idx rhs -> insertLet (kNormal arr) \lhs' ->
    insertLet (kNormal idx) \idx' ->
      insertLet (kNormal rhs) (pure . Put lhs' idx')
  where
    bind exprs constr =
      foldl'
        (\k m args -> insertLet m $ \arg -> k $ args <> [arg])
        (pure . constr)
        (kNormal <$> exprs)
