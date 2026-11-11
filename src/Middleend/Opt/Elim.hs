module Middleend.Opt.Elim (elimDeadCode) where

import Basic (Binder (..), FunBind (..), PrimOp (..))
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
  LetRec _ body -> isPure body
  LetTuple _ _ body -> isPure body
  App {} -> False
  where
    isPureOp = \case
      (Not {}; Neg {}; Add {}; Sub {}; Sali {}; Sari {}; FNeg {}; FAbs {}; FAdd {}; FSub {}; FMul {}; FDiv {}; FSqrt {}; Tuple {}; Array {}; Get {}; Floor {}; FloatOfInt {}; IntOfFloat {}) -> True
      (Put {}; In {}; Out {}) -> False

elimDeadCode :: (Log :> es) => Exp -> Eff es Exp
elimDeadCode = go
  where
    prune live binder kept body
      | live = pure kept
      | otherwise = do
          logMsg $ "eliminating variable: " <> show binder
          pure body

    go = \case
      e@(Unit; Int {}; Float {}; Var {}; PrimOp {}; App {}) -> pure e
      If cmp lhs rhs thn els -> If cmp lhs rhs <$> go thn <*> go els
      Let binder rhs body -> do
        rhs' <- go rhs
        body' <- go body
        prune (not (isPure rhs') || binder.name `S.member` (freeVars body')) binder (Let binder rhs' body') body'
      LetRec fb@FunBind {binder, rhs} body -> do
        rhs' <- go rhs
        body' <- go body
        prune (binder.name `S.member` (freeVars body')) binder (LetRec fb {rhs = rhs'} body') body'
      LetTuple binders rhs body -> do
        body' <- go body
        let bodyFv = freeVars body'
        prune (any (\b -> b.name `S.member` bodyFv) binders) binders (LetTuple binders rhs body') body'
