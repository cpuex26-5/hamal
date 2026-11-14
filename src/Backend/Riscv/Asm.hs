module Backend.Riscv.Asm (RegClass (..), SingClass (..), SomeClass (..), Operand (..), Block (..), Inst (..), FunDef (..), Program (..), XReg (..), FReg (..), idOf, dstOf) where

import Basic (Cmp (..), Rel)
import Buildable (Buildable (..))
import Data.ByteString.Builder (string7)
import Data.Char (toLower)
import Name (Id, Label)
import Pretty qualified as P
import Prettyprinter (Pretty (..), align, concatWith, hardline, nest, surround, tupled, (<+>))

data RegClass = GPR | FPR | Void
  deriving stock (Show, Eq)

data SingClass (c :: RegClass) where
  SGPR :: SingClass GPR
  SFPR :: SingClass FPR
  SVoid :: SingClass Void

deriving stock instance Show (SingClass c)

instance Pretty (SingClass c) where
  pretty = \case
    SGPR -> "gpr"
    SFPR -> "fpr"
    SVoid -> "void"

data SomeClass where
  SomeClass :: SingClass c -> SomeClass

data Operand (c :: RegClass) x f where
  OpX :: x -> Operand GPR x f
  OpF :: f -> Operand FPR x f

deriving stock instance (Show x, Show f) => Show (Operand c x f)

instance (Pretty x, Pretty f) => Pretty (Operand c x f) where
  pretty (OpX x) = pretty x
  pretty (OpF f) = pretty f

data Block (c :: RegClass) x f where
  Ans :: Inst c x f -> Block c x f
  Let :: Maybe (Operand c' x f) -> Inst c' x f -> Block c x f -> Block c x f

deriving stock instance (Show x, Show f) => Show (Block c x f)

instance (Pretty x, Pretty f) => Pretty (Block c x f) where
  pretty = \case
    Ans inst -> pretty inst
    Let Nothing inst body -> pretty inst <> hardline <> pretty body
    Let (Just dest) inst body -> pretty dest <+> "<-" <+> align (pretty inst) <> hardline <> pretty body

data Inst (c :: RegClass) x f where
  -- Common
  Nop :: Inst Void x f
  Mv :: Operand c x f -> Inst c x f
  Store :: Operand c x f -> x -> Int -> Inst Void x f
  StoreL :: Operand c x f -> Label -> Int -> Inst Void x f
  CallDir :: Label -> [x] -> [f] -> Inst c x f
  CallInd :: x -> [x] -> [f] -> Inst c x f
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

instance (Pretty x, Pretty f) => Pretty (Inst c x f) where
  pretty = \case
    Nop -> "nop"
    Mv rs -> P.insn "mv" [pretty rs]
    Store rs base offset -> P.insn "store" [pretty rs, P.mem offset base]
    StoreL rs label offset -> P.insn "storel" [pretty rs, P.mem offset label]
    CallDir label xargs fargs -> P.call "calldir" (pretty label) xargs fargs
    CallInd code xargs fargs -> P.call "callind" (pretty code) xargs fargs
    Save ident rs -> P.insn "save" [pretty ident, pretty rs]
    Alloc size -> P.insn "alloc" [pretty size]
    Out rs -> P.insn "out" [pretty rs]
    Li imm -> P.insn "li" [pretty imm]
    La label -> P.insn "la" [pretty label]
    Neg rs -> P.insn "neg" [pretty rs]
    Add rs1 rs2 -> P.insn "add" [pretty rs1, pretty rs2]
    Addi rs imm -> P.insn "addi" [pretty rs, pretty imm]
    Xori rs imm -> P.insn "xori" [pretty rs, pretty imm]
    Sub rs1 rs2 -> P.insn "sub" [pretty rs1, pretty rs2]
    Sali rs imm -> P.insn "sali" [pretty rs, pretty imm]
    Sari rs imm -> P.insn "sari" [pretty rs, pretty imm]
    If rel rs1 rs2 thn els -> P.ifThenElse (pretty rs1 <+> pretty (ICmp rel) <+> pretty rs2) (pretty thn) (pretty els)
    Lw offset base -> P.insn "lw" [P.mem offset base]
    LwL offset label -> P.insn "lwl" [P.mem offset label]
    Restore ident -> P.insn "restore" [pretty ident]
    FAdd rs1 rs2 -> P.insn "fadd" [pretty rs1, pretty rs2]
    FNeg rs -> P.insn "fneg" [pretty rs]
    FSub rs1 rs2 -> P.insn "fsub" [pretty rs1, pretty rs2]
    FMul rs1 rs2 -> P.insn "fmul" [pretty rs1, pretty rs2]
    FDiv rs1 rs2 -> P.insn "fdiv" [pretty rs1, pretty rs2]
    FSqrt rs -> P.insn "fsqrt" [pretty rs]
    FIf rel rs1 rs2 thn els -> P.ifThenElse (pretty rs1 <+> pretty (FCmp rel) <+> pretty rs2) (pretty thn) (pretty els)
    FLw offset base -> P.insn "flw" [P.mem offset base]
    FLwL offset label -> P.insn "flwl" [P.mem offset label]
    FRestore ident -> P.insn "frestore" [pretty ident]
    FloatOfInt rs -> P.insn "float_of_int" [pretty rs]
    IntOfFloat rs -> P.insn "int_of_float" [pretty rs]

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

instance (Pretty x, Pretty f) => Pretty (FunDef x f) where
  pretty FunDef {binder, xparams, fparams, rhs, ret} =
    nest 2 $
      "fundef"
        <+> pretty binder
        <+> tupled (pretty <$> xparams)
        <+> tupled (pretty <$> fparams)
        <+> "->"
        <+> pretty ret
        <+> "="
        <> hardline
        <> pretty rhs

data Program x f = Program [(Float, Label)] [FunDef x f] (Block Void x f)
  deriving stock (Show)

instance (Pretty x, Pretty f) => Pretty (Program x f) where
  pretty (Program floats fundefs main) =
    P.paragraphs $
      [concatWith (surround hardline) [pretty label <+> "=" <+> pretty float | (float, label) <- floats] | not (null floats)]
        <> (pretty <$> fundefs)
        <> [nest 2 ("main =" <> hardline <> pretty main)]

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

instance Pretty XReg where
  pretty = pretty . fmap toLower . show

instance Pretty FReg where
  pretty = pretty . fmap toLower . show

idOf :: Operand c x x -> x
idOf (OpX i) = i
idOf (OpF i) = i

dstOf :: Operand c Id Id -> Operand c XReg FReg
dstOf (OpX _) = OpX A0
dstOf (OpF _) = OpF Fa0
