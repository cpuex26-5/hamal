module Backend.Riscv.RegAlloc (regAlloc) where

import Backend.Riscv.Asm (Block (..), FReg (..), FunDef (..), Inst (..), Operand (..), Program (..), XReg (..), dstOf, idOf)
import Name (Id)

xargRegs :: [XReg]
xargRegs = [A0 .. A7]

fargRegs :: [FReg]
fargRegs = [Fa0 .. Fa7]

assignArgs :: [a] -> [b] -> [(a, b)]
assignArgs regs args
  | length regs <= length args = zip regs args
  | otherwise = error "assignArgs: too many arguments"

data Restoring where
  Restoring :: Operand c XReg FReg -> Id -> Restoring

restoring :: Restoring -> Block c XReg FReg -> Block c XReg FReg
restoring (Restoring op ident) = case op of
  OpX {} -> Let (Just op) (Restore ident)
  OpF {} -> Let (Just op) (FRestore ident)

allocFun :: FunDef Id Id -> FunDef XReg FReg
allocFun FunDef {binder, xargs, fargs, rhs, ret} =
  FunDef
    { binder
    , xargs = fst <$> xpairs
    , fargs = fst <$> fpairs
    , rhs = foldr (Let Nothing) (allocBlock rhs) saves
    , ret
    }
  where
    xpairs = assignArgs xargRegs xargs
    fpairs = assignArgs fargRegs fargs
    saves = [Save i (OpX x) | (x, i) <- xpairs] <> [Save i (OpF f) | (f, i) <- fpairs]

allocInst :: Inst c Id Id -> ([Restoring], Inst c XReg FReg)
allocInst = \case
  Nop -> pure Nop
  Li imm -> pure (Li imm)
  La label -> pure (La label)
  Mv (OpX x) -> Mv . OpX <$> rx T0 x
  Mv (OpF f) -> Mv . OpF <$> rf Ft0 f
  Neg x -> Neg <$> rx T0 x
  Add lhs rhs -> Add <$> rx T0 lhs <*> rx T1 rhs
  Addi lhs imm -> Addi <$> rx T0 lhs <*> pure imm
  Xori lhs imm -> Xori <$> rx T0 lhs <*> pure imm
  Sub lhs rhs -> Sub <$> rx T0 lhs <*> rx T1 rhs
  Sali x shamt -> Sali <$> rx T0 x <*> pure shamt
  Sari x shamt -> Sari <$> rx T0 x <*> pure shamt
  Lw offset addr -> Lw offset <$> rx T0 addr
  LwL offset label -> pure (LwL offset label)
  Store (OpX x) addr offset -> Store . OpX <$> rx T0 x <*> rx T1 addr <*> pure offset
  Store (OpF f) addr offset -> Store . OpF <$> rf Ft0 f <*> rx T1 addr <*> pure offset
  StoreL (OpX x) label offset -> StoreL . OpX <$> rx T0 x <*> pure label <*> pure offset
  StoreL (OpF f) label offset -> StoreL . OpF <$> rf Ft0 f <*> pure label <*> pure offset
  If rel lhs rhs thn els -> If rel <$> rx T0 lhs <*> rx T1 rhs <*> pure (allocBlock thn) <*> pure (allocBlock els)
  FIf rel lhs rhs thn els -> FIf rel <$> rf Ft0 lhs <*> rf Ft1 rhs <*> pure (allocBlock thn) <*> pure (allocBlock els)
  CallDir fun xprms fprms ->
    CallDir fun
      <$> traverse (uncurry rx) (assignArgs xargRegs xprms)
      <*> traverse (uncurry rf) (assignArgs fargRegs fprms)
  Save dst (OpX x) -> Save dst . OpX <$> rx T0 x
  Save dst (OpF f) -> Save dst . OpF <$> rf Ft0 f
  Restore ident -> pure $ Restore ident
  FNeg f -> FNeg <$> rf Ft0 f
  FAdd lhs rhs -> FAdd <$> rf Ft0 lhs <*> rf Ft1 rhs
  FSub lhs rhs -> FSub <$> rf Ft0 lhs <*> rf Ft1 rhs
  FMul lhs rhs -> FMul <$> rf Ft0 lhs <*> rf Ft1 rhs
  FDiv lhs rhs -> FDiv <$> rf Ft0 lhs <*> rf Ft1 rhs
  FSqrt f -> FSqrt <$> rf Ft0 f
  FLw offset addr -> FLw offset <$> rx T0 addr
  FLwL offset label -> pure (FLwL offset label)
  FRestore ident -> pure $ FRestore ident
  FloatOfInt x -> FloatOfInt <$> rx T0 x
  IntOfFloat f -> IntOfFloat <$> rf Ft0 f
  Out x -> Out <$> rx T0 x
  Alloc size -> pure (Alloc size)
  where
    rx x ident = ([Restoring (OpX x) ident], x)
    rf f ident = ([Restoring (OpF f) ident], f)

allocBlock :: Block c Id Id -> Block c XReg FReg
allocBlock = \case
  Ans inst -> foldr restoring (Ans inst') rs
    where
      (rs, inst') = allocInst inst
  Let dstM inst body -> withRest . withEval . withSave $ allocBlock body
    where
      (rs, inst') = allocInst inst
      withRest b = foldr restoring b rs -- inst のために T* に値を restore
      withEval = Let (dstOf <$> dstM) inst' -- inst を実行し、必要なら A* に bind
      withSave = maybe id (\b -> Let Nothing (Save (idOf b) (dstOf b))) dstM -- 必要なら A* から save

regAlloc :: Program Id Id -> Program XReg FReg
regAlloc (Program floats fundefs block) = Program floats (allocFun <$> fundefs) (allocBlock block)
