module Backend.Riscv.Asm (RegClass (..), SingClass (..), SomeClass (..), Operand (..), Block (..), Inst (..), FunDef (..), Program (..), XReg (..), FReg (..), idOf, dstOf) where

import Basic (Rel)
import Buildable (Buildable (..))
import Data.ByteString.Builder (string7)
import Data.Char (toLower)
import Name (Id, Label)

data RegClass = GPR | FPR | Void
  deriving stock (Show, Eq)

data SingClass (c :: RegClass) where
  SGPR :: SingClass GPR
  SFPR :: SingClass FPR
  SVoid :: SingClass Void

deriving stock instance Show (SingClass c)

data SomeClass where
  SomeClass :: SingClass c -> SomeClass

data Operand (c :: RegClass) x f where
  OpX :: x -> Operand GPR x f
  OpF :: f -> Operand FPR x f

deriving stock instance (Show x, Show f) => Show (Operand c x f)

data Block (c :: RegClass) x f where
  Ans :: Inst c x f -> Block c x f
  Let :: Maybe (Operand c' x f) -> Inst c' x f -> Block c x f -> Block c x f

deriving stock instance (Show x, Show f) => Show (Block c x f)

data Inst (c :: RegClass) x f where
  -- Common
  Nop :: Inst Void x f
  Mv :: Operand c x f -> Inst c x f
  Store :: Operand c x f -> x -> Int -> Inst Void x f
  StoreL :: Operand c x f -> Label -> Int -> Inst Void x f
  CallDir :: Label -> [x] -> [f] -> Inst c x f
  CallCls :: Label -> [x] -> [f] -> Inst c x f
  Save :: Id -> Operand c x f -> Inst Void x f
  Alloc :: Int -> Inst GPR x f -- dst <- hp; hp <- hp + n
  Out :: x -> Inst Void x f
  -- GPR
  Li :: Int -> Inst GPR x f
  La :: Label -> Inst GPR x f
  Neg :: x -> Inst GPR x f
  Add :: x -> x -> Inst GPR x f
  Addi :: x -> Int -> Inst GPR x f
  Xori :: x -> Int -> Inst GPR x f
  Sub :: x -> x -> Inst GPR x f
  Sali :: x -> Int -> Inst GPR x f
  Sari :: x -> Int -> Inst GPR x f
  If :: Rel -> x -> x -> Block c x f -> Block c x f -> Inst c x f
  Lw :: Int -> x -> Inst GPR x f
  LwL :: Int -> Label -> Inst GPR x f
  Restore :: Id -> Inst GPR x f
  -- FPR
  FAdd :: f -> f -> Inst FPR x f
  FNeg :: f -> Inst FPR x f
  FSub :: f -> f -> Inst FPR x f
  FMul :: f -> f -> Inst FPR x f
  FDiv :: f -> f -> Inst FPR x f
  FSqrt :: f -> Inst FPR x f
  FIf :: Rel -> f -> f -> Block c x f -> Block c x f -> Inst c x f
  FLw :: Int -> x -> Inst FPR x f
  FLwL :: Int -> Label -> Inst FPR x f
  FRestore :: Id -> Inst FPR x f
  FloatOfInt :: x -> Inst FPR x f
  IntOfFloat :: f -> Inst GPR x f

deriving stock instance (Show x, Show f) => Show (Inst c x f)

data FunDef x f where
  FunDef ::
    { binder :: Label
    , xparams :: [x]
    , fparams :: [f]
    , rhs :: Block c x f
    , ret :: SingClass c
    } ->
    FunDef x f

deriving stock instance (Show x, Show f) => Show (FunDef x f)

data Program x f = Program [(Float, Label)] [FunDef x f] (Block Void x f)
  deriving stock (Show)

data XReg
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

data FReg
  = Ft0
  | Ft1
  | Ft2
  | Ft3
  | Ft4
  | Ft5
  | Ft6
  | Ft7
  | Fs0
  | Fs1
  | Fa0
  | Fa1
  | Fa2
  | Fa3
  | Fa4
  | Fa5
  | Fa6
  | Fa7
  | Fs2
  | Fs3
  | Fs4
  | Fs5
  | Fs6
  | Fs7
  | Fs8
  | Fs9
  | Fs10
  | Fs11
  | Ft8
  | Ft9
  | Ft10
  | Ft11
  deriving stock (Show, Eq, Enum)

instance Buildable XReg where
  build = string7 . fmap toLower . show

instance Buildable FReg where
  build = string7 . fmap toLower . show

idOf :: Operand c x x -> x
idOf (OpX i) = i
idOf (OpF i) = i

dstOf :: Operand c Id Id -> Operand c XReg FReg
dstOf (OpX _) = OpX A0
dstOf (OpF _) = OpF Fa0
