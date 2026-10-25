module Backend.Riscv.Emit (emit) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Program (..), Reg (..), buildReg)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local, runReaderT)
import Control.Monad.Trans.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.ByteString.Builder qualified as BB
import Data.Map qualified as M
import Id (Fresh, Id, buildName, fresh, mkName, toLabel)

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
  Li imm -> tell $ "\tli\t" <> buildReg rd <> ", " <> BB.intDec imm <> "\n"
  La label -> tell $ "\tla\t" <> buildReg rd <> ", " <> buildName label <> "\n"
  Mv rs -> tell $ "\tmv\t" <> buildReg rd <> ", " <> buildReg rs <> "\n"
  Add rs1 rs2 -> tell $ "\tadd\t" <> buildReg rd <> ", " <> buildReg rs1 <> ", " <> buildReg rs2 <> "\n"
  Addi rs1 imm -> tell $ "\taddi\t" <> buildReg rd <> ", " <> buildReg rs1 <> ", " <> BB.intDec imm <> "\n"
  Neg rs -> tell $ "\tsub\t" <> buildReg rd <> ", " <> buildReg Zero <> ", " <> buildReg rs <> "\n"
  Sub rs1 rs2 -> tell $ "\tsub\t" <> buildReg rd <> ", " <> buildReg rs1 <> ", " <> buildReg rs2 <> "\n"
  Lw offset rs -> tell $ "\tlw\t" <> buildReg rd <> ", " <> BB.intDec offset <> "(" <> buildReg rs <> ")\n"
  Lwl offset label ->
    tell ("\tla\t" <> buildReg T0 <> ", " <> buildName label <> "\n")
      *> tell ("\tlw\t" <> buildReg rd <> ", " <> BB.intDec offset <> "(" <> buildReg T0 <> ")\n")
  Sw rd' rs offset -> tell $ "\tsw\t" <> buildReg rd' <> ", " <> BB.intDec offset <> "(" <> buildReg rs <> ")\n"
  IfEq lhs rhs thn els -> do
    label_else <- fmap toLabel <$> lift . lift $ fresh "else"
    label_cont <- fmap toLabel <$> lift . lift $ fresh "cont"
    tell $ "\tbne\t" <> buildReg lhs <> ", " <> buildReg rhs <> ", " <> buildName label_else <> "\n"
    emitBlock (NonTail rd) thn
    tell $ "\tj\t" <> buildName label_cont <> "\n"
    tell $ buildName label_else <> ":\n"
    emitBlock (NonTail rd) els
    tell $ buildName label_cont <> ":\n"
  IfLe lhs rhs thn els -> do
    label_else <- fmap toLabel <$> lift . lift $ fresh "else"
    label_cont <- fmap toLabel <$> lift . lift $ fresh "cont"
    tell $ "\tbgt\t" <> buildReg lhs <> ", " <> buildReg rhs <> ", " <> buildName label_else <> "\n"
    emitBlock (NonTail rd) thn
    tell $ "\tj\t" <> buildName label_cont <> "\n"
    tell $ buildName label_else <> ":\n"
    emitBlock (NonTail rd) els
    tell $ buildName label_cont <> ":\n"
  CallDir fun _ -> tell $ "\tcall\t" <> buildName fun <> "\n"
  Save rs ident -> do
    offset <- lift (asks \frame -> frame.slots M.! ident)
    tell $ "\tsw\t" <> buildReg rs <> ", " <> BB.intDec offset <> "(" <> buildReg Sp <> ")\n"
  Restore ident -> do
    offset <- lift (asks \frame -> frame.slots M.! ident)
    tell $ "\tlw\t" <> buildReg rd <> ", " <> BB.intDec offset <> "(" <> buildReg Sp <> ")\n"

emitBlock :: Dest -> Block Reg -> Emit ()
emitBlock dest = \case
  Ans inst -> case dest of
    Tail -> do
      emitNonTail A0 inst
      size <- lift $ asks (.size)
      tell $ "\tlw\t" <> buildReg Ra <> ", " <> BB.intDec (size - 4) <> "(" <> buildReg Sp <> ")\n"
      tell $ "\taddi\t" <> buildReg Sp <> ", " <> buildReg Sp <> ", " <> BB.intDec size <> "\n"
      tell "\tret\n"
    NonTail reg -> emitNonTail reg inst
  Let reg inst body -> emitNonTail reg inst *> emitBlock dest body

emitFun :: FunDef Reg -> Emit ()
emitFun FunDef {name, rhs} = do
  let frame = mkFrame rhs
  tell $ buildName name <> ":\n"
  tell $ "\taddi\t" <> buildReg Sp <> ", " <> buildReg Sp <> ", -" <> BB.intDec frame.size <> "\n"
  tell $ "\tsw\t" <> buildReg Ra <> ", " <> BB.intDec (frame.size - 4) <> "(" <> buildReg Sp <> ")\n"
  mapWriterT (local (const frame)) $ emitBlock Tail rhs

emit :: Program Reg -> Fresh BB.Builder
emit (Program fundefs block) = flip runReaderT emptyFrame $ execWriterT do
  tell ".section\t.text\n"
  mapM_ emitFun fundefs
  tell ".global\tmin_caml_start\n"
  emitFun (FunDef (mkName "min_caml_start") [] block)
  where
    emptyFrame = Frame {slots = M.empty, size = 0}
