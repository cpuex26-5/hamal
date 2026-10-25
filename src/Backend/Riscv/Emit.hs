module Backend.Riscv.Emit (emit) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..), Reg (..))
import Buildable (Buildable (build))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local, runReaderT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.ByteString.Builder qualified as BB
import Data.Map qualified as M
import Id (Fresh, Id, fresh, mkName, toLabel)

data Frame = Frame {slots :: M.Map Id Int, size :: Int}

type Emit = WriterT BB.Builder (ReaderT Frame Fresh)

data Dest = Tail | NonTail Reg

mkFrame :: Block r -> Frame
mkFrame block = Frame {slots, size = align $ (M.size slots + 1) * 4} -- 1 for ra
  where
    slots = M.fromList $ zip (saveB block) [0, 4 ..]
    align n = ((n + 15) `div` 16) * 16

    saveB :: Block r -> [Id]
    saveB = \case
      Ans inst -> saveI inst
      Let _ rhs body -> saveI rhs <> saveB body
    saveI :: Inst r -> [Id]
    saveI = \case
      Nop; Li {}; La {}; Mv {}; Add {}; Addi {}; Neg {}; Sub {}; Lw {}; Lwl {}; Sw {}; CallDir {}; Restore {} -> []
      IfEq _ _ thn els -> saveB thn <> saveB els
      IfLe _ _ thn els -> saveB thn <> saveB els
      Save _ ident -> [ident]

emitNonTail :: Reg -> Inst Reg -> Emit ()
emitNonTail rd = \case
  Nop -> pure ()
  Li imm -> tell $ "\tli\t" <> build rd <> ", " <> BB.intDec imm <> "\n"
  La label -> tell $ "\tla\t" <> build rd <> ", " <> build label <> "\n"
  Mv rs -> tell $ "\tmv\t" <> build rd <> ", " <> build rs <> "\n"
  Add rs1 rs2 -> tell $ "\tadd\t" <> build rd <> ", " <> build rs1 <> ", " <> build rs2 <> "\n"
  Addi rs1 imm -> tell $ "\taddi\t" <> build rd <> ", " <> build rs1 <> ", " <> BB.intDec imm <> "\n"
  Neg rs -> tell $ "\tsub\t" <> build rd <> ", " <> build Zero <> ", " <> build rs <> "\n"
  Sub rs1 rs2 -> tell $ "\tsub\t" <> build rd <> ", " <> build rs1 <> ", " <> build rs2 <> "\n"
  Lw offset rs -> tell $ "\tlw\t" <> build rd <> ", " <> BB.intDec offset <> "(" <> build rs <> ")\n"
  Lwl offset label ->
    tell ("\tla\t" <> build T0 <> ", " <> build label <> "\n")
      *> tell ("\tlw\t" <> build rd <> ", " <> BB.intDec offset <> "(" <> build T0 <> ")\n")
  Sw rd' rs offset -> tell $ "\tsw\t" <> build rd' <> ", " <> BB.intDec offset <> "(" <> build rs <> ")\n"
  IfEq lhs rhs thn els -> do
    label_else <- fmap toLabel <$> lift . lift $ fresh "else"
    label_cont <- fmap toLabel <$> lift . lift $ fresh "cont"
    tell $ "\tbne\t" <> build lhs <> ", " <> build rhs <> ", " <> build label_else <> "\n"
    emitBlock (NonTail rd) thn
    tell $ "\tj\t" <> build label_cont <> "\n"
    tell $ build label_else <> ":\n"
    emitBlock (NonTail rd) els
    tell $ build label_cont <> ":\n"
  IfLe lhs rhs thn els -> do
    label_else <- fmap toLabel <$> lift . lift $ fresh "else"
    label_cont <- fmap toLabel <$> lift . lift $ fresh "cont"
    tell $ "\tbgt\t" <> build lhs <> ", " <> build rhs <> ", " <> build label_else <> "\n"
    emitBlock (NonTail rd) thn
    tell $ "\tj\t" <> build label_cont <> "\n"
    tell $ build label_else <> ":\n"
    emitBlock (NonTail rd) els
    tell $ build label_cont <> ":\n"
  CallDir fun _ -> tell $ "\tcall\t" <> build fun <> "\n"
  Save rs ident -> do
    offset <- lift (asks \frame -> frame.slots M.! ident)
    tell $ "\tsw\t" <> build rs <> ", " <> BB.intDec offset <> "(" <> build Sp <> ")\n"
  Restore ident -> do
    offset <- lift (asks \frame -> frame.slots M.! ident)
    tell $ "\tlw\t" <> build rd <> ", " <> BB.intDec offset <> "(" <> build Sp <> ")\n"

emitBlock :: Dest -> Block Reg -> Emit ()
emitBlock dest = \case
  Ans inst -> case dest of
    Tail -> do
      emitNonTail A0 inst
      size <- lift $ asks (.size)
      tell $ "\tlw\t" <> build Ra <> ", " <> BB.intDec (size - 4) <> "(" <> build Sp <> ")\n"
      tell $ "\taddi\t" <> build Sp <> ", " <> build Sp <> ", " <> BB.intDec size <> "\n"
      tell "\tret\n"
    NonTail reg -> emitNonTail reg inst
  Let reg inst body -> emitNonTail reg inst *> emitBlock dest body

emitFun :: FunDef Reg -> Emit ()
emitFun FunDef {name, rhs} = do
  let frame = mkFrame rhs
  tell $ build name <> ":\n"
  tell $ "\taddi\t" <> build Sp <> ", " <> build Sp <> ", -" <> BB.intDec frame.size <> "\n"
  tell $ "\tsw\t" <> build Ra <> ", " <> BB.intDec (frame.size - 4) <> "(" <> build Sp <> ")\n"
  mapWriterT (local (const frame)) $ emitBlock Tail rhs

emit :: Program Reg -> Fresh BB.Builder
emit (Program fundefs block) = flip runReaderT emptyFrame $ execWriterT do
  tell ".section\t.text\n"
  mapM_ emitFun fundefs
  tell ".global\tmin_caml_start\n"
  emitFun (FunDef (mkName "min_caml_start") [] block)
  where
    emptyFrame = Frame {slots = M.empty, size = 0}
