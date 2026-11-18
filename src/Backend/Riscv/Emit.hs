module Backend.Riscv.Emit (emit) where

import Backend.Riscv.Asm (Block (..), FReg (..), FunDef (..), Inst (..), Operand (..), Program (..), RegClass (..), SingClass (..), XReg (..))
import Basic (Rel (..))
import Buildable (Buildable (build))
import Data.Bool (bool)
import Data.ByteString.Builder qualified as BB
import Data.Foldable (for_)
import Data.List (intersperse)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Writer.Static.Local (Writer, execWriter, tell)
import Name (Fresh, Id, Label (..), fresh)

data Frame = Frame {slots :: M.Map Id Int, size :: Int}
  deriving stock (Show)

data Dest c where
  Tail :: Bool -> SingClass c -> Dest c
  NonTail :: Maybe (Operand c XReg FReg) -> Dest c

regHp :: XReg
regHp = Gp

mkFrame :: Block c x f -> Frame
mkFrame block = Frame {slots, size = align $ (M.size slots + 1) * 4} -- 1 for ra
  where
    slots = M.fromList $ zip (S.toList (saveB block)) [0, 4 ..]
    align n = ((n + 15) `div` 16) * 16
    saveB = \case
      Ans inst -> saveI inst
      Let _ rhs body -> saveI rhs <> saveB body
    saveI :: Inst c x f -> S.Set Id
    saveI = \case
      If _ _ _ thn els -> saveB thn <> saveB els
      Save ident _ -> S.singleton ident
      _ -> S.empty

emitI :: (Writer BB.Builder :> es) => BB.Builder -> [BB.Builder] -> Eff es ()
emitI op args = tell $ "\t" <> op <> "\t" <> mconcat (intersperse ", " args) <> "\n"

emitL :: (Writer BB.Builder :> es) => Label -> Eff es ()
emitL label = tell $ build label <> ":\n"

memFormat :: (Buildable a) => Int -> a -> BB.Builder
memFormat offset base = BB.intDec offset <> "(" <> build base <> ")"

emitNonTail :: (Fresh :> es, Writer BB.Builder :> es, Reader Frame :> es) => Maybe (Operand c XReg FReg) -> Inst c XReg FReg -> Eff es ()
emitNonTail rdM = \case
  Nop -> pure ()
  Li imm -> for_ (xreg rdM) \rd -> emitI "li" [build rd, BB.intDec imm]
  La label -> for_ (xreg rdM) \rd -> emitI "la" [build rd, build label]
  Mv rs -> case rs of
    OpX x -> for_ (xreg rdM) \rd -> emitI "mv" [build rd, build x]
    OpF f -> for_ (freg rdM) \rd -> emitI "fmv.s" [build rd, build f]
  Neg rs -> for_ (xreg rdM) \rd -> emitI "sub" [build rd, build Zero, build rs]
  Add rs1 rs2 -> for_ (xreg rdM) \rd -> emitI "add" [build rd, build rs1, build rs2]
  Addi rs1 imm -> for_ (xreg rdM) \rd -> emitI "addi" [build rd, build rs1, BB.intDec imm]
  Xor rs1 rs2 -> for_ (xreg rdM) \rd -> emitI "xor" [build rd, build rs1, build rs2]
  Xori rs1 imm -> for_ (xreg rdM) \rd -> emitI "xori" [build rd, build rs1, BB.intDec imm]
  Sub rs1 rs2 -> for_ (xreg rdM) \rd -> emitI "sub" [build rd, build rs1, build rs2]
  Slli rs imm -> for_ (xreg rdM) \rd -> emitI "slli" [build rd, build rs, BB.intDec imm]
  Srai rs imm -> for_ (xreg rdM) \rd -> emitI "srai" [build rd, build rs, BB.intDec imm]
  Lw offset rs -> for_ (xreg rdM) \rd -> emitI "lw" [build rd, memFormat offset rs]
  Store rs addr offset -> case rs of
    OpX x -> emitI "sw" [build x, memFormat offset addr]
    OpF f -> emitI "fsw" [build f, memFormat offset addr]
  Save ident rs -> do
    offset <- asks @Frame (M.findWithDefault (error "emitNonTail(unreachable)") ident . (.slots))
    case rs of
      OpX x -> emitI "sw" [build x, memFormat offset Sp]
      OpF f -> emitI "fsw" [build f, memFormat offset Sp]
  Restore ident -> for_ (xreg rdM) \rd -> do
    offset <- asks @Frame (M.findWithDefault (error "emitNonTail(unreachable)") ident . (.slots))
    emitI "lw" [build rd, memFormat offset Sp]
  If rel lhs rhs thn els -> do
    label_else <- Local <$> fresh "else"
    label_cont <- Local <$> fresh "cont"
    let b = case rel of
          Eq -> "bne"
          Neq -> "beq"
          Lt -> "bge"
          Le -> "bgt"
          Gt -> "ble"
          Ge -> "blt"
    emitI b [build lhs, build rhs, build label_else]
    emitBlock (NonTail rdM) thn
    emitI "j" [build label_cont]
    emitL label_else
    emitBlock (NonTail rdM) els
    emitI "j" [build label_cont]
    emitL label_cont
  CallDir fun _ _ -> do
    emitI "call" [build fun]
    for_ rdM \case
      OpX x -> emitI "mv" [build x, build A0]
      OpF f -> emitI "fmv.s" [build f, build Fa0]
  CallInd code _ _ -> do
    emitI "jalr" [build code]
    for_ rdM \case
      OpX x -> emitI "mv" [build x, build A0]
      OpF f -> emitI "fmv.s" [build f, build Fa0]
  FNeg rs -> for_ (freg rdM) \rd -> emitI "fneg.s" [build rd, build rs]
  FAdd lhs rhs -> for_ (freg rdM) \rd -> emitI "fadd.s" [build rd, build lhs, build rhs]
  FSub lhs rhs -> for_ (freg rdM) \rd -> emitI "fsub.s" [build rd, build lhs, build rhs]
  FMul lhs rhs -> for_ (freg rdM) \rd -> emitI "fmul.s" [build rd, build lhs, build rhs]
  FDiv lhs rhs -> for_ (freg rdM) \rd -> emitI "fdiv.s" [build rd, build lhs, build rhs]
  Feq lhs rhs -> for_ (xreg rdM) \rd -> emitI "feq.s" [build rd, build lhs, build rhs]
  Flt lhs rhs -> for_ (xreg rdM) \rd -> emitI "flt.s" [build rd, build lhs, build rhs]
  Fle lhs rhs -> for_ (xreg rdM) \rd -> emitI "fle.s" [build rd, build lhs, build rhs]
  FSqrt rs -> for_ (freg rdM) \rd -> emitI "fsqrt.s" [build rd, build rs]
  FAbs rs -> for_ (freg rdM) \rd -> emitI "fabs.s" [build rd, build rs]
  FLw offset addr -> for_ (freg rdM) \rd -> emitI "flw" [build rd, memFormat offset addr]
  FRestore ident -> for_ (freg rdM) \rd -> do
    offset <- asks @Frame (M.findWithDefault (error "emitNonTail(unreachable)") ident . (.slots))
    emitI "flw" [build rd, memFormat offset Sp]
  FloatOfInt rs -> for_ (freg rdM) \rd -> emitI "fcvt.s.w" [build rd, build rs]
  IntOfFloat rs -> for_ (xreg rdM) \rd -> emitI "fcvt.w.s" [build rd, build rs]
  FloatOfBits rs -> for_ (freg rdM) \rd -> emitI "fmv.w.x" [build rd, build rs]
  BitsOfFloat rs -> for_ (xreg rdM) \rd -> emitI "fmv.x.w" [build rd, build rs]
  In base -> emitI "lw" [build (fromMaybe Zero (xreg rdM)), memFormat 4 base]
  Out base rs -> emitI "sw" [build rs, memFormat 0 base]
  Alloc size -> do
    emitI "addi" [build regHp, build regHp, BB.intDec size]
    for_ (xreg rdM) \rd -> emitI "addi" [build rd, build regHp, BB.intDec (-size)]
  AllocDyn rs -> do
    emitI "add" [build regHp, build regHp, build rs]
    for_ (xreg rdM) \rd -> emitI "sub" [build rd, build regHp, build rs]
  where
    xreg :: Maybe (Operand GPR XReg FReg) -> Maybe XReg
    xreg = fmap \(OpX r) -> r
    freg :: Maybe (Operand FPR XReg FReg) -> Maybe FReg
    freg = fmap \(OpF r) -> r

emitBlock :: (Fresh :> es, Writer BB.Builder :> es, Reader Frame :> es) => Dest c -> Block c XReg FReg -> Eff es ()
emitBlock dest = \case
  Ans inst -> case dest of
    Tail ebreak c -> do
      case c of
        SGPR -> emitNonTail (Just (OpX A0)) inst
        SFPR -> emitNonTail (Just (OpF Fa0)) inst
        SVoid -> emitNonTail Nothing inst
      size <- asks @Frame (.size)
      emitI "lw" [build Ra, memFormat (size - 4) Sp]
      emitI "addi" [build Sp, build Sp, BB.intDec size]
      tell @BB.Builder $ bool "\tret\n" "\tebreak\n" ebreak
    NonTail reg -> emitNonTail reg inst
  Let rd inst body -> emitNonTail rd inst *> emitBlock dest body

emitFun :: (Fresh :> es, Writer BB.Builder :> es) => Bool -> FunDef XReg FReg -> Eff es ()
emitFun ebreak FunDef {binder, rhs, ret} = do
  let frame = mkFrame rhs
  emitL binder
  emitI "addi" [build Sp, build Sp, "-" <> BB.intDec frame.size]
  emitI "sw" [build Ra, memFormat (frame.size - 4) Sp]
  runReader frame $ emitBlock (Tail ebreak ret) rhs

emit :: (Fresh :> es) => Program XReg FReg -> Eff es BB.Builder
emit (Program fundefs block) = execWriter do
  tell @BB.Builder ".section\t.text\n"
  tell @BB.Builder ".global\tmin_caml_start\n"
  emitFun True main
  mapM_ (emitFun False) fundefs
  -- runtime(min_caml_fill)
  emitL $ Global "min_caml_fill" -- a0 = addr, a1 = n, a2 = bits
  emitI "slli" [build A1, build A1, BB.intDec 2]
  emitI "add" [build A1, build A1, build A0]
  emitI "bge" [build A0, build A1, "min_caml_fill.end"]
  emitL $ Global "min_caml_fill.loop"
  emitI "sw" [build A2, memFormat 0 A0]
  emitI "addi" [build A0, build A0, BB.intDec 4]
  emitI "blt" [build A0, build A1, "min_caml_fill.loop"]
  emitL $ Global "min_caml_fill.end"
  emitI "ret" []
  where
    main =
      FunDef
        { binder = Global "min_caml_start"
        , xparams = []
        , fparams = []
        , rhs = block
        , ret = SVoid
        }
