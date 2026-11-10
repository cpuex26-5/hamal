module Middleend.Opt.Elim (elimDeadCode) where

import Basic (
  Binder (..),
  FunBind (..),
  PrimOp (..),
 )
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Log (Log, logMsg)
import Middleend.KNormal (Exp (..), freeVars)

isPure :: Exp -> Bool
isPure = \case
  (Unit; Int {}; Float {}; Var {}) -> True
  PrimOp op -> isPureOp op
  If _ _ _ thn els -> isPure thn && isPure els
  Let _ rhs body -> isPure rhs && isPure body
  LetRec FunBind {rhs} body -> isPure rhs && isPure body
  LetTuple _ _ body -> isPure body
  App {} -> False
  where
    isPureOp = \case
      (Not {}; Neg {}; FNeg {}; Add {}; Sub {}; Sali {}; Sari {}; FAdd {}; FSub {}; FMul {}; FDiv {}; FSqrt {}; Tuple {}; Array {}; Get {}; FloatOfInt {}; IntOfFloat {}) -> True
      (Put {}; Out {}) -> False

elimDeadCode :: (Log :> es) => Exp -> Eff es Exp
elimDeadCode = go
  where
    go = \case
      e@(Unit; Int {}; Float {}; Var {}; PrimOp {}; App {}) -> pure e
      If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      Let binder rhs body -> do
        rhs' <- go rhs
        body' <- go body
        if (not (isPure rhs') || binder.name `S.member` (freeVars body'))
          then
            pure (Let binder rhs' body')
          else do
            logMsg $ "eliminating variable: " <> show binder
            pure body'
      LetRec fb@FunBind {binder, rhs} body -> do
        rhs' <- go rhs
        body' <- go body
        if (binder.name `S.member` (freeVars body'))
          then pure $ LetRec fb {rhs = rhs'} body'
          else do
            logMsg $ "eliminating variable: " <> show binder
            pure body'
      LetTuple binders rhs body -> do
        body' <- go body
        let bodyFv = freeVars body'
        if (any (\b -> b.name `S.member` bodyFv) binders)
          then
            pure $ LetTuple binders rhs body'
          else do
            logMsg $ "eliminating variable: " <> show binders
            pure body'
