module Backend.Riscv.Asm (Block (..), Inst (..), FunDef (..), Program (..), Reg (..)) where

import Buildable (Buildable (..))
import Data.ByteString.Builder (intDec)
import Name (Id, Label)
import PrimOp (Cmp)

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
  | If Cmp r r (Block r) (Block r)
  | CallDir Label [r]
  | Save r Id
  | Restore Id
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
  = Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | T0
  | T1
  | T2
  | S0
  | S1
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | T3
  | T4
  | T5
  | T6
  deriving stock (Show, Eq, Enum)

instance Buildable Reg where
  build = ("x" <>) . intDec . fromEnum
