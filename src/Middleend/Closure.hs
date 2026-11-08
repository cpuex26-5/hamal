module Middleend.Closure (Closure (..), Exp (..), FunDef (..), Program (..), closureConvert) where

import Basic (Binder (..), Cmp, FunBind (..), PrimOp)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Map.Strict as M hiding (null)
import Data.Set qualified as S
import Effectful (Eff, runPureEff, (:>))
import Effectful.Output.Static.Local.List (Output, output, runOutput)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Middleend.KNormal qualified as K
import Name (Id, Label (Local))
import Type (Type)

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
  | Let (Binder Id Type) Exp Exp
  | LetTuple [Binder Id Type] Id Exp
  | -- \| MakeCls Id Closure Exp
    -- \| AppCls Id [Id]
    AppDir Label [Id]
  -- \| ExtArray Label
  deriving stock (Eq, Show)

data FunDef = FunDef
  { binder :: Binder Label Type
  , params :: [Binder Id Type]
  , formalFv :: [Binder Id Type]
  , rhs :: Exp
  }
  deriving stock (Eq, Show)

data Program = Program [FunDef] Exp
  deriving stock (Eq, Show)

freeVars :: Exp -> S.Set Id
freeVars = \case
  (Unit; Int _; Float _) -> S.empty
  Var ident -> S.singleton ident
  PrimOp op -> foldMap S.singleton op
  If _ lhs rhs thn els -> S.singleton lhs <> S.singleton rhs <> freeVars thn <> freeVars els
  Let binder rhs body -> S.difference (freeVars rhs <> freeVars body) $ S.singleton binder.name
  AppDir _ args -> S.fromList args
  LetTuple binders rhs body -> S.difference (S.singleton rhs <> freeVars body) $ S.fromList ((.name) <$> binders)

closureConvert :: K.Exp -> Program
closureConvert = uncurry (flip Program) . runPureEff . runOutput . runReader (M.empty :: M.Map Id Type) . go
  where
    go :: (Output FunDef :> es, Reader (M.Map Id Type) :> es) => K.Exp -> Eff es Exp
    go = \case
      K.Unit -> pure Unit
      K.Int int -> pure $ Int int
      K.Float float -> pure $ Float float
      K.Var ident -> pure $ Var ident
      K.PrimOp op -> pure $ PrimOp op
      K.If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      K.Let binder rhs body -> Let binder <$> go rhs <*> local (with binder) (go body)
      K.LetRec FunBind {binder, params, rhs} body -> do
        rhs' <- local (foldMap with (binder : params)) (go rhs)
        let fv = S.toList $ S.difference (freeVars rhs') (S.fromList $ (.name) <$> binder : params)
        body' <- local (with binder) (go body)
        formalFv <- asks (\env -> (\name -> Binder {name, ty = M.findWithDefault (error $ "closure(unreachable)" <> show name) name env}) <$> fv)
        unless (null formalFv) (error "closure found")
        output $ FunDef {binder = first Local binder, params, formalFv, rhs = rhs'}
        pure body'
      K.LetTuple binders rhs body -> LetTuple binders rhs <$> local (foldMap with binders) (go body)
      K.App fun args -> pure $ AppDir (Local fun) args
    with binder = M.insert binder.name binder.ty
