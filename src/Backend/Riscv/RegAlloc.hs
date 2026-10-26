module Backend.Riscv.RegAlloc (regAlloc) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..), Reg (..))
import Data.Bifunctor (Bifunctor (bimap))
import Name (Id)

regAlloc :: Program Id -> Program Reg
regAlloc (Program fundefs block) = Program (allocFun <$> fundefs) (allocBlock block)
  where
    binding = zip (enumFrom A0) -- TODO: assertion
    restoring = foldr (\(reg, ident) -> Let reg (Restore ident))
    storing = foldr (\(reg, ident) -> Let Zero (Save reg ident))

    allocFun :: FunDef Id -> FunDef Reg
    allocFun FunDef {name, args, rhs} =
      FunDef
        { name
        , args = fst <$> binding args
        , rhs = storing (allocBlock rhs) (binding args)
        }

    allocInst :: Inst Id -> (Inst Reg, [Id])
    allocInst = \case
      Nop -> (Nop, [])
      Li imm -> (Li imm, [])
      La label -> (La label, [])
      Mv ident -> (Mv T0, [ident])
      Add lhs rhs -> (Add T0 T1, [lhs, rhs])
      Addi lhs imm -> (Addi T0 imm, [lhs])
      Neg ident -> (Neg T0, [ident])
      Sub lhs rhs -> (Sub T0 T1, [lhs, rhs])
      Lw offset addr -> (Lw offset T0, [addr])
      Lwl offset label -> (Lwl offset label, [])
      Sw dst src offset -> (Sw T0 T1 offset, [dst, src])
      IfEq lhs rhs thn els -> (IfEq T0 T1 (allocBlock thn) (allocBlock els), [lhs, rhs])
      IfLe lhs rhs thn els -> (IfLe T0 T1 (allocBlock thn) (allocBlock els), [lhs, rhs])
      CallDir fun params -> (CallDir fun (fst <$> binding params), params)
      Save dst src -> (Save T0 src, [dst])
      Restore ident -> (Restore ident, [])

    allocBlock :: Block Id -> Block Reg
    allocBlock = \case
      Ans inst -> withRestore T0 Ans inst
      Let binder inst@CallDir {} body -> withRestore A0 (\i -> Let A0 i (Let Zero (Save A0 binder) (allocBlock body))) inst
      Let binder inst body -> withRestore T0 (\i -> Let T0 i (Let Zero (Save T0 binder) (allocBlock body))) inst
      where
        withRestore startReg toBlock = uncurry restoring . bimap toBlock (zip (enumFrom startReg)) . allocInst
