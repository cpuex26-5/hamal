module Backend.Riscv.Emit (emit) where

import Backend.Riscv.Asm (Block (..), FReg (..), FunDef (..), Inst (..), Operand (..), Program (..), RegClass (..), SingClass (..), XReg (..))
import Basic (Rel (..))
import Buildable (Buildable (build))
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Bool (bool)
import Data.ByteString.Builder (string7)
import Data.ByteString.Builder qualified as BB
import Data.Foldable (for_)
import Data.List (intersperse)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Writer.Static.Local (Writer, execWriter, tell)
import GHC.Float (castFloatToWord32)
import Name (Fresh, Id, Label (..), fresh)
import Text.Printf (printf)

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
      FIf _ _ _ thn els -> saveB thn <> saveB els
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
  LwL offset label -> for_ (xreg rdM) \rd -> do
    emitI "la" [build T1, build label]
    emitI "lw" [build rd, memFormat offset T1]
  Store rs addr offset -> case rs of
    OpX x -> emitI "sw" [build x, memFormat offset addr]
    OpF f -> emitI "fsw" [build f, memFormat offset addr]
  StoreL rs label offset -> do
    emitI "la" [build T1, build label]
    case rs of
      OpX x -> emitI "sw" [build x, memFormat offset T1]
      OpF f -> emitI "fsw" [build f, memFormat offset T1]
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
  FIf rel lhs rhs thn els -> do
    label_else <- Local <$> fresh "else"
    label_cont <- Local <$> fresh "cont"
    let s = case rel of
          Eq -> "feq.s"
          Neq -> "fneq.s"
          Lt -> "flt.s"
          Le -> "fle.s"
          Gt -> "fgt.s"
          Ge -> "fge.s"
    emitI s [build T1, build lhs, build rhs]
    emitI "beqz" [build T1, build label_else]
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
  Fli imm -> for_ (freg rdM) \rd -> do
    let bits = castFloatToWord32 imm
    let high = bits `shiftR` 12
    let low = fromIntegral (bits .&. 0xfff) - fromIntegral (bits .&. (1 `shiftL` 11))
    emitI "lui" [build T0, string7 $ printf "0x%05x" high]
    emitI "addi" [build T0, build T0, BB.intDec low]
    emitI "fmv.w.x" [build rd, build T0]
  FNeg rs -> for_ (freg rdM) \rd -> emitI "fneg.s" [build rd, build rs]
  FAdd lhs rhs -> for_ (freg rdM) \rd -> emitI "fadd.s" [build rd, build lhs, build rhs]
  FSub lhs rhs -> for_ (freg rdM) \rd -> emitI "fsub.s" [build rd, build lhs, build rhs]
  FMul lhs rhs -> for_ (freg rdM) \rd -> emitI "fmul.s" [build rd, build lhs, build rhs]
  FDiv lhs rhs -> for_ (freg rdM) \rd -> emitI "fdiv.s" [build rd, build lhs, build rhs]
  FSqrt rs -> for_ (freg rdM) \rd -> emitI "fsqrt.s" [build rd, build rs]
  FAbs rs -> for_ (freg rdM) \rd -> emitI "fabs.s" [build rd, build rs]
  Floor rs -> for_ (freg rdM) \rd -> do
    label <- Local <$> fresh "floorL"
    emitI "fcvt.w.s" [build T3, build rs]
    emitI "fcvt.s.w" [build rd, build T3]
    emitI "fle.s" [build T4, build rd, build rs]
    emitI "bnez" [build T4, build label]
    emitI "addi" [build T3, build T3, BB.intDec (-1)]
    emitI "fcvt.s.w" [build rd, build T3]
    emitL label
  FLw offset addr -> for_ (freg rdM) \rd -> emitI "flw" [build rd, memFormat offset addr]
  FLwL offset label -> for_ (freg rdM) \rd -> do
    emitI "la" [build T1, build label]
    emitI "flw" [build rd, memFormat offset T1]
  FRestore ident -> for_ (freg rdM) \rd -> do
    offset <- asks @Frame (M.findWithDefault (error "emitNonTail(unreachable)") ident . (.slots))
    emitI "flw" [build rd, memFormat offset Sp]
  FloatOfInt rs -> for_ (freg rdM) \rd -> emitI "fcvt.s.w" [build rd, build rs]
  IntOfFloat rs -> for_ (xreg rdM) \rd -> emitI "fcvt.w.s" [build rd, build rs]
  FloatOfBits rs -> for_ (freg rdM) \rd -> emitI "fmv.w.x" [build rd, build rs]
  BitsOfFloat rs -> for_ (xreg rdM) \rd -> emitI "fmv.x.w" [build rd, build rs]
  In -> do
    let rd = fromMaybe Zero (xreg rdM)
    emitI "lui" [build T1, "0x" <> BB.int32HexFixed mmioBase]
    emitI "lw" [build rd, memFormat 4 T1]
  Out rs -> do
    emitI "lui" [build T1, "0x" <> BB.int32HexFixed mmioBase]
    emitI "sw" [build rs, memFormat 0 T1]
  Alloc size -> do
    for_ (xreg rdM) \rd -> emitI "mv" [build rd, build regHp]
    emitI "addi" [build regHp, build regHp, BB.intDec size]
  Fill base size initial -> do
    emitI "slli" [build size, build size, BB.intDec 2]
    emitI "add" [build size, build size, build base]
    loop <- Local <$> fresh "fill.start"
    loopEnd <- Local <$> fresh "fill.end"
    emitI "beq" [build base, build size, build loopEnd]
    emitL loop
    case initial of
      OpX x -> emitI "sw" [build x, memFormat 0 base]
      OpF f -> emitI "fsw" [build f, memFormat 0 base]
    emitI "addi" [build base, build base, BB.intDec 4]
    emitI "blt" [build base, build size, build loop]
    emitL loopEnd
  MakeArray size initial -> for_ (xreg rdM) \rd -> do
    emitI "mv" [build rd, build regHp]
    emitI "mv" [build T3, build regHp]
    emitI "slli" [build size, build size, BB.intDec 2]
    emitI "add" [build regHp, build regHp, build size]
    loop <- Local <$> fresh "makearray.start"
    loopEnd <- Local <$> fresh "makearray.end"
    emitI "beqz" [build size, build loopEnd]
    emitL loop
    case initial of
      OpX x -> emitI "sw" [build x, memFormat 0 T3]
      OpF f -> emitI "fsw" [build f, memFormat 0 T3]
    emitI "addi" [build T3, build T3, BB.intDec 4]
    emitI "blt" [build T3, build regHp, build loop]
    emitL loopEnd
  where
    xreg :: Maybe (Operand GPR XReg FReg) -> Maybe XReg
    xreg = fmap \(OpX r) -> r
    freg :: Maybe (Operand FPR XReg FReg) -> Maybe FReg
    freg = fmap \(OpF r) -> r
    mmioBase = 0xffff0

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
  where
    main =
      FunDef
        { binder = Global "min_caml_start"
        , xparams = []
        , fparams = []
        , rhs = block
        , ret = SVoid
        }
