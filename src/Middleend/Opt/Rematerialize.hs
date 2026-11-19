module Middleend.Opt.Rematerialize (constRemat) where

import Basic (
  Binder (..),
  FunBind (..),
 )
import Control.Monad (unless)
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (asks, local, runReader)
import Log (Log, logMsg)
import Middleend.KNormal (Exp (..), freeVars)
import Name (Fresh, Id, refresh)
import Type qualified as T

data Value = VInt Int | VFloat Float

type VEnv = M.Map Id Value

type REnv = M.Map Id Id

constRemat :: (Fresh :> es, Log :> es) => Exp -> Eff es Exp
constRemat = runReader @REnv M.empty . runReader @VEnv M.empty . go
  where
    lookupM = asks @VEnv . M.lookup
    rename x = asks @REnv (M.findWithDefault x x)
    valueOf = \case
      Int n -> pure $ Just (VInt n)
      Float f -> pure $ Just (VFloat f)
      Var x -> lookupM x
      _ -> pure Nothing

    go = \case
      e@(Unit; Int {}; Float {}) -> pure e
      Var x -> Var <$> rename x
      PrimOp op -> PrimOp <$> traverse rename op
      App fun params -> App <$> rename fun <*> traverse rename params
      If cnd lhs rhs thn els -> If cnd <$> rename lhs <*> rename rhs <*> go thn <*> go els
      Let binder rhs body -> do
        rhs' <- go rhs
        valueM <- valueOf rhs'
        Let binder rhs' <$> maybe id (local . M.insert binder.name) valueM (go body)
      LetTuple binders rhs body -> LetTuple binders <$> rename rhs <*> go body
      LetRec fb@FunBind {rhs} body -> do
        let fv = toList $ freeVars fb.rhs
        values <- traverse lookupM fv
        refreshes <- traverse refresh fv
        let remating = catMaybes $ zipWith3 (\old new mv -> (old,new,) <$> mv) fv refreshes values
            renaming = [(old, new) | (old, new, _) <- remating]
        rhs' <- local @REnv (M.union (M.fromList renaming)) (go rhs)
        let rhs'' =
              foldr
                ( \(name, v) k -> case v of
                    VInt n -> Let (Binder name (T.Type T.Int)) (Int n) k
                    VFloat f -> Let (Binder name (T.Type T.Float)) (Float f) k
                )
                rhs'
                [(new, value) | (_, new, value) <- remating]
        unless (null renaming) . logMsg $
          "rematerializing in " <> show fb.binder <> ": " <> show renaming
        LetRec fb {rhs = rhs''} <$> go body
