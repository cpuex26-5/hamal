module Middleend.Opt.ConstFold (constFold) where

import Basic (Binder (..), Cmp (..), PrimOp (..), compareOf)
import Control.Monad (join)
import Data.Bits (shiftL, shiftR, xor)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Effectful (Eff)
import Effectful.Reader.Static (asks, local, runReader)
import Middleend.KNormal (Exp (..))
import Name (Id)

data Value = VInt Int | VFloat Float | VTuple [Id]

type Env = M.Map Id Value

constFold :: Exp -> Eff es Exp
constFold = runReader @Env M.empty . go
  where
    lookupM = asks . M.lookup

    valueOf = \case
      Int n -> pure $ Just (VInt n)
      Float f -> pure $ Just (VFloat f)
      PrimOp (Tuple xs) -> pure $ Just (VTuple xs)
      Var x -> lookupM x
      _ -> pure Nothing

    go = \case
      e@(Unit; Int {}; Float {}; App {}) -> pure e
      e@(Var x) ->
        lookupM x <&> \case
          Just (VInt n) -> Int n
          Just (VFloat f) -> Float f
          Just (VTuple {}) -> e
          Nothing -> e
      e@(PrimOp op) -> do
        args <- traverse lookupM op
        pure $ fromMaybe e (evalOp =<< sequenceA args)
      If cmp lhs rhs thn els -> do
        cnd <- liftA2 (evalCmp cmp) <$> lookupM lhs <*> lookupM rhs
        thn' <- go thn
        els' <- go els
        pure $ case join cnd of
          Just True -> thn'
          Just False -> els'
          Nothing -> If cmp lhs rhs thn' els'
      Let binder rhs body -> do
        rhs' <- go rhs
        val <- valueOf rhs'
        Let binder rhs' <$> maybe id (local . M.insert binder.name) val (go body)
      LetRec fb body -> LetRec <$> traverse go fb <*> go body
      LetTuple binders x body ->
        lookupM x >>= \case
          Just (VTuple xs) -> go $ foldr (\(b, y) -> Let b (Var y)) body (zip binders xs)
          _ -> LetTuple binders x <$> go body

asInt :: Value -> Maybe Int
asInt = \case
  VInt n -> Just n
  _ -> Nothing

asFloat :: Value -> Maybe Float
asFloat = \case
  VFloat f -> Just f
  _ -> Nothing

evalOp :: PrimOp Value -> Maybe Exp
evalOp = \case
  Not i -> Int . (1 -) <$> asInt i
  Xor lhs rhs -> Int <$> liftA2 xor (asInt lhs) (asInt rhs)
  Neg i -> Int . negate <$> asInt i
  Add lhs rhs -> Int <$> liftA2 (+) (asInt lhs) (asInt rhs)
  Sub lhs rhs -> Int <$> liftA2 (-) (asInt lhs) (asInt rhs)
  Slli i n -> Int . (`shiftL` n) <$> asInt i
  Srai i n -> Int . (`shiftR` n) <$> asInt i
  FNeg f -> Float . negate <$> asFloat f
  FAdd lhs rhs -> Float <$> liftA2 (+) (asFloat lhs) (asFloat rhs)
  FSub lhs rhs -> Float <$> liftA2 (-) (asFloat lhs) (asFloat rhs)
  FMul lhs rhs -> Float <$> liftA2 (*) (asFloat lhs) (asFloat rhs)
  FDiv lhs rhs -> Float <$> liftA2 (/) (asFloat lhs) (asFloat rhs)
  FSqrt f -> Float . sqrt <$> asFloat f
  FAbs f -> Float . abs <$> asFloat f
  Floor f -> Float . (fromInteger . floor) <$> asFloat f
  FloatOfInt i -> Float . fromIntegral <$> asInt i
  IntOfFloat f -> Int . round <$> asFloat f
  (Tuple {}; Array {}; Get {}; Put {}; In {}; Out {}; Coerce {}) -> Nothing

evalCmp :: Cmp -> Value -> Value -> Maybe Bool
evalCmp cmp l r = case cmp of
  ICmp rel -> compareOf rel <$> asInt l <*> asInt r
  FCmp rel -> compareOf rel <$> asFloat l <*> asFloat r
