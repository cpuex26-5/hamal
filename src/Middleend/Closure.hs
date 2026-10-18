module Middleend.Closure (Closure (..), Expr (..), FunDef (..), Program (..), closureConvert) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Set qualified as S
import Id (Id, Label, constantify)
import Middleend.KNormal (FunBind (FunBind))
import Middleend.KNormal qualified as K

data Closure = Closure
  { entry :: Label
  , actualFv :: [Id]
  }
  deriving stock (Eq, Show)

data Expr
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
  | IfEq Id Id Expr Expr
  | IfLe Id Id Expr Expr
  | Let Id Expr Expr
  | Var Id
  | MakeCls Id Closure Expr
  | AppCls Id [Id]
  | -- | AppDir Label [Id]
    Tuple [Id]
  | LetTuple [Id] Id Expr
  | Get Id Id
  | Put Id Id Id
  -- \| ExtArray Label
  deriving stock (Eq, Show)

data FunDef = FunDef
  { name :: Label
  , args :: [Id]
  , formalFv :: [Id]
  , rhs :: Expr
  }
  deriving stock (Eq, Show)

data Program = Program [FunDef] Expr
  deriving stock (Eq, Show)

-- TODO: Direct App
closureConvert :: K.KNormal -> Program
closureConvert = uncurry (flip Program) . runWriter . go
  where
    go :: K.KNormal -> Writer [FunDef] Expr
    go = \case
      K.Unit -> pure Unit
      K.Int int -> pure $ Int int
      K.Float float -> pure $ Float float
      K.Neg ident -> pure $ Neg ident
      K.Add lhs rhs -> pure $ Add lhs rhs
      K.Sub lhs rhs -> pure $ Sub lhs rhs
      K.FNeg ident -> pure $ FNeg ident
      K.FAdd lhs rhs -> pure $ FAdd lhs rhs
      K.FSub lhs rhs -> pure $ FSub lhs rhs
      K.FMul lhs rhs -> pure $ FMul lhs rhs
      K.FDiv lhs rhs -> pure $ FDiv lhs rhs
      K.IfEq lhs rhs thn els -> IfEq lhs rhs <$> go thn <*> go els
      K.IfLe lhs rhs thn els -> IfLe lhs rhs <$> go thn <*> go els
      K.Let binder rhs body -> Let binder <$> go rhs <*> go body
      K.Var ident -> pure $ Var ident
      K.LetRec FunBind {name, args, rhs} body -> do
        rhs' <- go rhs
        let fv = S.toList $ S.difference (freeVars rhs') (S.fromList $ name : args)
        let closure = Closure {entry = constantify name, actualFv = fv}
        body' <- go body
        tell [FunDef {name = constantify name, args, formalFv = fv, rhs = rhs'}]
        pure $ MakeCls name closure body'
      K.App fun params -> pure $ AppCls fun params
      K.Tuple idents -> pure $ Tuple idents
      K.LetTuple binders rhs body -> LetTuple binders rhs <$> go body
      K.Get arr idx -> pure $ Get arr idx
      K.Put arr idx rhs -> pure $ Put arr idx rhs

    freeVars = \case
      (Unit; Int _; Float _) -> S.empty
      Neg ident -> S.singleton ident
      Add lhs rhs -> S.fromList [lhs, rhs]
      Sub lhs rhs -> S.fromList [lhs, rhs]
      FAdd lhs rhs -> S.fromList [lhs, rhs]
      FSub lhs rhs -> S.fromList [lhs, rhs]
      FNeg ident -> S.singleton ident
      FMul lhs rhs -> S.fromList [lhs, rhs]
      FDiv lhs rhs -> S.fromList [lhs, rhs]
      IfEq lhs rhs thn els -> S.fromList [lhs, rhs] <> freeVars thn <> freeVars els
      IfLe lhs rhs thn els -> S.fromList [lhs, rhs] <> freeVars thn <> freeVars els
      Let binder rhs body -> S.difference (freeVars rhs <> freeVars body) $ S.singleton binder
      Var ident -> S.singleton ident
      MakeCls binder Closure {actualFv} body -> S.delete binder $ S.fromList actualFv <> freeVars body
      AppCls fun params -> S.fromList $ fun : params
      Tuple idents -> S.fromList idents
      LetTuple binders rhs body -> S.difference (S.singleton rhs <> freeVars body) $ S.fromList binders
      Get arr idx -> S.fromList [arr, idx]
      Put arr idx rhs -> S.fromList [arr, idx, rhs]
