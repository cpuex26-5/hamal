module Middleend.Closure (Closure (..), Exp (..), FunDef (..), Program (..), closureConvert) where

import Data.Set qualified as S
import Effectful (Eff, runPureEff, (:>))
import Effectful.Output.Static.Local.List (Output, output, runOutput)
import Middleend.KNormal (FunBind (FunBind))
import Middleend.KNormal qualified as K
import Name (Id, Label, toLabel)
import PrimOp (Cmp, PrimOp)

data Closure = Closure
  { entry :: Label
  , actualFv :: [Id]
  }
  deriving stock (Eq, Show)

data Exp
  = Unit
  | Int Int
  | Float Float
  | Var Id
  | PrimOp (PrimOp Id)
  | If Cmp Id Id Exp Exp
  | Let Id Exp Exp
  | LetTuple [Id] Id Exp
  | -- \| MakeCls Id Closure Exp
    -- \| AppCls Id [Id]
    AppDir Label [Id]
  -- \| ExtArray Label
  deriving stock (Eq, Show)

data FunDef = FunDef
  { name :: Label
  , args :: [Id]
  , formalFv :: [Id]
  , rhs :: Exp
  }
  deriving stock (Eq, Show)

data Program = Program [FunDef] Exp
  deriving stock (Eq, Show)

closureConvert :: K.Exp -> Program
closureConvert = uncurry (flip Program) . runPureEff . runOutput . go
  where
    go :: (Output FunDef :> es) => K.Exp -> Eff es Exp
    go = \case
      K.Unit -> pure Unit
      K.Int int -> pure $ Int int
      K.Float float -> pure $ Float float
      K.Var ident -> pure $ Var ident
      K.PrimOp op -> pure $ PrimOp op
      K.If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      K.Let binder rhs body -> Let binder <$> go rhs <*> go body
      K.LetRec FunBind {name, args, rhs} body -> do
        rhs' <- go rhs
        let fv = S.toList $ S.difference (freeVars rhs') (S.fromList $ name : args)
        body' <- go body
        output $ FunDef {name = toLabel name, args, formalFv = fv, rhs = rhs'}
        pure body'
      K.LetTuple binders rhs body -> LetTuple binders rhs <$> go body
      K.App fun params -> pure $ AppDir (toLabel fun) params

    freeVars = \case
      (Unit; Int _; Float _) -> S.empty
      Var ident -> S.singleton ident
      PrimOp op -> foldMap S.singleton op
      If _ lhs rhs thn els -> S.fromList [lhs, rhs] <> freeVars thn <> freeVars els
      Let binder rhs body -> S.difference (freeVars rhs <> freeVars body) $ S.singleton binder
      -- MakeCls binder Closure {actualFv} body -> S.delete binder $ S.fromList actualFv <> freeVars body
      -- AppCls fun params -> S.fromList $ fun : params
      AppDir _ params -> S.fromList params
      LetTuple binders rhs body -> S.difference (S.singleton rhs <> freeVars body) $ S.fromList binders
