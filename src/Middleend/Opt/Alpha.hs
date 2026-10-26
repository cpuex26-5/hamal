module Middleend.Opt.Alpha (alphaConvert) where

import Data.Map qualified as M
import Effectful ((:>))
import Effectful.Internal.Monad (Eff)
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Middleend.KNormal (Exp (..), FunBind (..))
import Name (Fresh, Id, refresh)

alphaConvert :: (Fresh :> es) => Exp -> Eff es Exp
alphaConvert = runReader @(M.Map Id Id) M.empty . go
  where
    go :: (Fresh :> es, Reader (M.Map Id Id) :> es) => Exp -> Eff es Exp
    go expr = do
      resolve <- asks (\m ident -> M.findWithDefault ident ident m)
      case expr of
        e@(Unit; Int _; Float _) -> pure e
        PrimOp op -> pure $ PrimOp (resolve <$> op)
        If cmp lhs rhs thn els -> If cmp (resolve lhs) (resolve rhs) <$> go thn <*> go els
        Let binder rhs body -> do
          binder' <- refresh binder
          rhs' <- go rhs
          body' <- local (M.insert binder binder') $ go body
          pure $ Let binder' rhs' body'
        Var ident -> pure . Var . resolve $ ident
        LetRec FunBind {name, args, rhs} body -> do
          name' <- refresh name
          args' <- traverse refresh args
          rhs' <- local (\m -> foldl' (flip $ uncurry M.insert) m $ zip args args') $ go rhs
          body' <- local (M.insert name name') $ go body
          pure $ LetRec FunBind {name = name', args = args', rhs = rhs'} body'
        App fun params -> pure $ App (resolve fun) (resolve <$> params)
        LetTuple binders rhs body -> do
          binders' <- traverse refresh binders
          body' <- local (\m -> foldl' (flip $ uncurry M.insert) m $ zip binders binders') $ go body
          pure $ LetTuple binders' (resolve rhs) body'
