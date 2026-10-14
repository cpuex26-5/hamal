module Frontend.KNormal (KNormal (..), kNormal, prettyKNormal) where

import Control.Monad.Trans.State (State, evalState)
import Data.Bool (bool)
import Data.ByteString.Builder (intDec, toLazyByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.RealFloat (floatDec)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (intersperse)
import Data.Semigroup (stimes)
import Frontend.Id (Counter, Id (..), buildId, freshId, initialCounter)
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

prettyKNormal :: KNormal -> BS.ByteString
prettyKNormal = toLazyByteString . go 0
  where
    go :: Int -> KNormal -> BB.Builder
    go level expr =
      stimes (level * 2) " "
        <> case expr of
          Unit -> "Unit"
          Int int -> spaced ["Int", intDec int]
          Float float -> spaced ["Float", floatDec float]
          Neg ident -> spaced ["Neg", buildId ident]
          Add lhs rhs -> spaced ["Add", buildId lhs, buildId rhs]
          Sub lhs rhs -> spaced ["Sub", buildId lhs, buildId rhs]
          FNeg ident -> spaced ["FNeg", buildId ident]
          FAdd lhs rhs -> spaced ["FAdd", buildId lhs, buildId rhs]
          FSub lhs rhs -> spaced ["FSub", buildId lhs, buildId rhs]
          FMul lhs rhs -> spaced ["FMul", buildId lhs, buildId rhs]
          FDiv lhs rhs -> spaced ["FDiv", buildId lhs, buildId rhs]
          IfEq lhs rhs thn els -> spaced ["IfEq", buildId lhs, buildId rhs] <> "\n" <> go (level + 1) thn <> "\n" <> go (level + 1) els
          IfLe lhs rhs thn els -> spaced ["IfLe", buildId lhs, buildId rhs] <> "\n" <> go (level + 1) thn <> "\n" <> go (level + 1) els
          Let binder rhs body -> spaced ["Let", buildId binder] <> "\n" <> go (level + 1) rhs <> "\n" <> go (level + 1) body
          Var ident -> spaced ["Var", buildId ident]
          LetRec FunBind {name, args, rhs} body -> spaced (["LetRec", buildId name] <> (buildId <$> args)) <> "\n" <> go (level + 1) rhs <> "\n" <> go (level + 1) body
          App fun params -> spaced $ ["App", buildId fun] <> (buildId <$> params)
          Tuple idents -> spaced $ "Tuple" : (buildId <$> idents)
          LetTuple binders rhs body -> spaced ("LetTuple" : (buildId <$> binders) <> [buildId rhs]) <> "\n" <> go (level + 1) body
          Get arr idx -> spaced ["Get", buildId arr, buildId idx]
          Put arr idx rhs -> spaced ["Put", buildId arr, buildId idx, buildId rhs]

    spaced = mconcat . intersperse " "

type M = State Counter

insertLet :: M KNormal -> (Id -> M KNormal) -> M KNormal
insertLet scrutM k = do
  scrut <- scrutM
  case scrut of
    Var x -> k x
    _ -> do
      fresh <- freshId
      body <- k fresh
      pure $ Let fresh scrut body

kNormal :: Syntax a -> KNormal
kNormal expr = evalState (go expr) initialCounter
  where
    go :: Syntax a -> M KNormal
    go Syntax {ann, kind} = case kind of
      S.Unit -> pure Unit
      S.Int int -> pure $ Int int
      S.Float float -> pure $ Float float
      S.Bool bol -> pure . Int $ bool 0 1 bol
      S.Not e -> go $ Syntax ann $ S.If e (Syntax ann (S.Bool False)) (Syntax ann (S.Bool True))
      S.Neg e -> insertLet (go e) (pure . Neg)
      S.FNeg e -> insertLet (go e) (pure . FNeg)
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
          pure $ App (Id "create_array") [len', initial']
      S.Get arr idx -> insertLet (go arr) \lhs' ->
        insertLet (go idx) (pure . Get lhs')
      S.Put arr idx rhs -> insertLet (go arr) \lhs' ->
        insertLet (go idx) \idx' ->
          insertLet (go rhs) (pure . Put lhs' idx')

    bind exprs constr =
      foldl'
        (\k m args -> insertLet m $ \arg -> k $ args <> [arg])
        (pure . constr)
        (go <$> exprs)
