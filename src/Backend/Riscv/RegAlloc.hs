module Backend.Riscv.RegAlloc (regAlloc) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..), Reg (..))
import Basic (Binder (..))
import Data.Bifunctor (Bifunctor (bimap))
import Name (Id)
import Type qualified as T

regAlloc :: Program Id -> Program Reg
regAlloc (Program fundefs block) = Program (allocFun <$> fundefs) (allocBlock block)
  where
    binding = zip (enumFrom A0) -- TODO: assertion
    restoring = foldr (\(reg, Binder {name, ty}) -> Let (Binder reg ty) (Restore name))
    saving reg ident = Let (Binder Zero (T.Type T.Unit)) (Save reg ident)
    storing = foldr (uncurry saving)

    allocFun :: FunDef Id -> FunDef Reg
    allocFun FunDef {binder, args, rhs, ret} =
      FunDef
        { binder
        , args = fst <$> binding args
        , rhs = storing (allocBlock rhs) (binding args)
        , ret
        }

    allocInst :: Inst Id -> (Inst Reg, [Binder Id T.Type])
    allocInst = \case
      Nop -> (Nop, [])
      Li imm -> (Li imm, [])
      La label -> (La label, [])
      Mv ident -> (Mv T0, [int ident])
      Add lhs rhs -> (Add T0 T1, [int lhs, int rhs])
      Addi lhs imm -> (Addi T0 imm, [int lhs])
      Neg ident -> (Neg T0, [int ident])
      Sub lhs rhs -> (Sub T0 T1, [int lhs, int rhs])
      Lw offset addr -> (Lw offset T0, [int addr])
      Lwl offset label -> (Lwl offset label, [])
      Sw dst src offset -> (Sw T0 T1 offset, [int dst, int src])
      If cmp lhs rhs thn els -> (If cmp T0 T1 (allocBlock thn) (allocBlock els), [int lhs, int rhs])
      CallDir fun params -> (CallDir fun (fst <$> binding params), int <$> params)
      Save dst src -> (Save T0 src, [int dst])
      Restore ident -> (Restore ident, [])
      where
        int ident = Binder ident (T.Type T.Int)

    allocBlock :: Block Id -> Block Reg
    allocBlock = \case
      Ans inst -> withRestore T0 Ans inst
      Let binder inst@CallDir {} body ->
        withRestore A0 (\i -> Let (Binder A0 binder.ty) i (saving A0 binder.name (allocBlock body))) inst
      Let binder inst body ->
        withRestore T0 (\i -> Let (Binder T0 binder.ty) i (saving T0 binder.name (allocBlock body))) inst
      where
        withRestore startReg toBlock = uncurry restoring . bimap toBlock (zip (enumFrom startReg)) . allocInst
