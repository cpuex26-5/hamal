module Backend.Riscv.Virtual (genVirtual) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..))
import Effectful (Eff, (:>))
import Middleend.Closure qualified as C
import Name (Fresh, Id)
import PrimOp qualified as P

virtualBlock :: (Fresh :> es) => C.Exp -> Eff es (Block Id)
virtualBlock = \case
  C.Unit -> pure $ Ans Nop
  C.Int int -> pure $ Ans (Li int)
  C.Float _float -> undefined
  C.Var ident -> pure . Ans . Mv $ ident
  C.PrimOp op -> case op of
    P.Neg e -> pure . Ans . Neg $ e
    P.FNeg _ -> undefined
    P.Add lhs rhs -> pure . Ans $ Add lhs rhs
    P.Sub lhs rhs -> pure . Ans $ Sub lhs rhs
    _ -> undefined
  C.If cmp lhs rhs thn els -> (\thn' els' -> Ans $ If cmp lhs rhs thn' els') <$> virtualBlock thn <*> virtualBlock els
  C.Let binder rhs body -> insertLet binder <$> virtualBlock rhs <*> virtualBlock body
  C.LetTuple _binders _rhs _body -> undefined
  C.AppDir fun params -> pure . Ans $ CallDir fun params
  where
    insertLet binder rhs body = case rhs of
      Ans inst -> Let binder inst body
      Let binder' rhs' body' -> Let binder' rhs' $ insertLet binder body' body

virtualFunc :: (Fresh :> es) => C.FunDef -> Eff es (FunDef Id)
virtualFunc C.FunDef {name, args, formalFv, rhs} = do
  rhs' <- virtualBlock rhs
  let (_, load) = foldr (\fv (offset, rest) -> (offset + 4, Let fv (Lwl offset name) rest)) (4, rhs') formalFv
  pure $ FunDef {name, args, rhs = load}

genVirtual :: (Fresh :> es) => C.Program -> Eff es (Program Id)
genVirtual (C.Program fundefs expr) = Program <$> traverse virtualFunc fundefs <*> virtualBlock expr
