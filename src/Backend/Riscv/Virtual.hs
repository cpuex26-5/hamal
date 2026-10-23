module Backend.Riscv.Virtual (genVirtual) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..))
import Id (Fresh)
import Middleend.Closure qualified as C

virtualBlock :: C.Exp -> Fresh Block
virtualBlock = \case
  C.Unit -> pure $ Ans Nop
  C.Int int -> pure $ Ans (Li int)
  C.Float _float -> undefined
  C.Neg expr -> pure . Ans . Neg $ expr
  C.Add lhs rhs -> pure . Ans $ Add lhs rhs
  C.Sub lhs rhs -> pure . Ans $ Sub lhs rhs
  C.FNeg _expr -> undefined
  C.FAdd _lhs _rhs -> undefined
  C.FSub _lhs _rhs -> undefined
  C.FMul _lhs _rhs -> undefined
  C.FDiv _lhs _rhs -> undefined
  C.IfEq lhs rhs thn els -> (\thn' els' -> Ans $ IfEq lhs rhs thn' els') <$> virtualBlock thn <*> virtualBlock els
  C.IfLe lhs rhs thn els -> (\thn' els' -> Ans $ IfLe lhs rhs thn' els') <$> virtualBlock thn <*> virtualBlock els
  C.Let binder rhs body -> insertLet binder <$> virtualBlock rhs <*> virtualBlock body
  C.Var ident -> pure . Ans . Mv $ ident
  -- C.MakeCls binder C.Closure {entry, actualFv} body -> do
  --   body' <- virtualBlock body
  --   (offset, storedFreeVars) <- foldM (\(offset, rest) fv -> (offset + 4,) <$> sequenced (Sw fv binder offset) rest) (4, body') actualFv
  --   label <- Id.refresh (Id "l")
  --   inner <- sequenced (Sw label binder 0) storedFreeVars
  --   pure
  --     $ Let binder (Mv regHeap)
  --     $ Let regHeap (Addi regHeap offset)
  --     $ Let label (La entry) inner
  -- C.AppCls fun params -> pure . Ans $ CallCls fun params
  C.AppDir fun params -> pure . Ans $ CallDir fun params
  C.Tuple _idents -> undefined
  C.LetTuple _binders _rhs _body -> undefined
  C.Get _arr _idx -> undefined
  C.Put _arr _idx _rhs -> undefined
  where
    -- sequenced e1 e2 = Let <$> Id.freshTmpId <*> pure e1 <*> pure e2
    insertLet binder rhs body = case rhs of
      Ans inst -> Let binder inst body
      Let binder' rhs' body' -> Let binder' rhs' $ insertLet binder body' body

virtualFunc :: C.FunDef -> Fresh FunDef
virtualFunc C.FunDef {name, args, formalFv, rhs} = do
  rhs' <- virtualBlock rhs
  let (_, load) = foldr (\fv (offset, rest) -> (offset + 4, Let fv (Lwl offset name) rest)) (4, rhs') formalFv
  pure $ FunDef {name, args, rhs = load}

genVirtual :: C.Program -> Fresh Program
genVirtual (C.Program fundefs expr) = Program <$> traverse virtualFunc fundefs <*> virtualBlock expr
