module Frontend.KNormal (KNormal (..), FunBind (..), kNormal) where

import Data.Bool (bool)
import Frontend.Id (Fresh, Id (..), freshTmpId)
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
  -- \| ExtArray Id
  -- \| ExtFunApp Id [Id]
  deriving stock (Show, Eq)

data FunBind = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: KNormal
  }
  deriving stock (Show, Eq)

insertLet :: Fresh KNormal -> (Id -> Fresh KNormal) -> Fresh KNormal
insertLet scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      fresh <- freshTmpId
      body <- k fresh
      pure $ Let fresh scrut body

kNormal :: Syntax a -> Fresh KNormal
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
      pure $ App (Id "create_array") [len', initial']
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
