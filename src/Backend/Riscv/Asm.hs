module Backend.Riscv.Asm (Block (..), Inst (..), FunDef (..), Program (..), Reg (..)) where

import Id (Label)

data Block r
  = Ans (Inst r)
  | Let r (Inst r) (Block r)
  deriving stock (Show, Eq)

data Inst r
  = Nop
  | Li Int
  | La Label
  | Mv r
  | Add r r
  | Addi r Int
  | Neg r
  | Sub r r
  | Lw Int r
  | Lwl Int Label
  | Sw r r Int
  | IfEq r r (Block r) (Block r)
  | IfLe r r (Block r) (Block r)
  | CallDir Label [r]
  deriving stock (Show, Eq)

data FunDef r = FunDef
  { name :: Label
  , args :: [r]
  , rhs :: Block r
  }
  deriving stock (Show, Eq)

data Program r = Program [FunDef r] (Block r)
  deriving stock (Show, Eq)

data Reg
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | X31
  deriving stock (Eq, Enum)

instance Show Reg where
  show r = "x" <> show (fromEnum r)
