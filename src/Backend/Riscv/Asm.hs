module Backend.Riscv.Asm (Block (..), Inst (..), FunDef (..), Program (..), regHeap) where

import Id (Id (..), Label)

data Block
  = Ans Inst
  | Let Id Inst Block
  deriving stock (Show, Eq)

data Inst
  = Nop
  | Li Int
  | La Label
  | Mv Id
  | Add Id Id
  | Addi Id Int
  | Neg Id
  | Sub Id Id
  | Lw Int Id
  | Sw Id Id Int
  | IfEq Id Id Block Block
  | IfLe Id Id Block Block
  | CallCls Id [Id]
  deriving stock (Show, Eq)

data FunDef = FunDef
  { name :: Label
  , args :: [Id]
  , rhs :: Block
  }
  deriving stock (Show, Eq)

data Program = Program [FunDef] Block
  deriving stock (Show, Eq)

regHeap :: Id
regHeap = Id "x31"
