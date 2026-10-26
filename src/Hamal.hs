module Hamal (Dump (..), Phase (..), Stage (..), phase, compile, CompileError (..), phaseName) where

import Backend.Riscv.Asm (Reg)
import Backend.Riscv.Asm qualified as V
import Backend.Riscv.Emit (emit)
import Backend.Riscv.RegAlloc (regAlloc)
import Backend.Riscv.Virtual (genVirtual)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error, throwError)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)
import Frontend.SrcSpan (SrcSpan)
import Frontend.Syntax (Syntax)
import Middleend.Closure (closureConvert)
import Middleend.Closure qualified as C
import Middleend.KNormal (kNormal)
import Middleend.KNormal qualified as K
import Middleend.Opt.Alpha (alphaConvert)
import Middleend.Opt.Assoc (assocLet)
import Middleend.Opt.Beta (betaConvert)
import Name (Id, runFresh)

data Phase
  = Parsed
  | KNormal
  | Alpha
  | Beta
  | Assoc
  | Closure
  | Virtual
  | RegAlloc
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Stage a where
  SParsed :: Stage (Syntax SrcSpan)
  SKNormal :: Stage K.Exp
  SAlpha :: Stage K.Exp
  SBeta :: Stage K.Exp
  SAssoc :: Stage K.Exp
  SClosure :: Stage C.Program
  SVirtual :: Stage (V.Program Id)
  SRegAlloc :: Stage (V.Program Reg)

phase :: Stage a -> Phase
phase = \case
  SParsed -> Parsed
  SKNormal -> KNormal
  SAlpha -> Alpha
  SBeta -> Beta
  SAssoc -> Assoc
  SClosure -> Closure
  SVirtual -> Virtual
  SRegAlloc -> RegAlloc

phaseName :: Phase -> String
phaseName = \case
  Parsed -> "parsed"
  KNormal -> "knorm"
  Alpha -> "alpha"
  Beta -> "beta"
  Assoc -> "assoc"
  Closure -> "closure"
  Virtual -> "virtual"
  RegAlloc -> "regalloc"

data Dump :: Effect where
  Dump :: (Show a) => Stage a -> a -> Dump m ()

type instance DispatchOf Dump = Dynamic

newtype CompileError = ParseError String
  deriving newtype (Show, Eq)

stage :: (Show a, Dump :> es) => Stage a -> Eff es a -> Eff es a
stage s m = do
  x <- m
  send (Dump s x)
  pure x

compile :: (Error CompileError :> es, Dump :> es) => ByteString -> Eff es Builder
compile input = runFresh do
  syntax <- stage SParsed . either (throwError . ParseError) pure $ runAlex input parse
  knorm <- stage SKNormal . kNormal $ syntax
  alpha <- stage SAlpha . alphaConvert $ knorm
  beta <- stage SBeta . pure . betaConvert $ alpha
  assoc <- stage SAssoc . pure . assocLet $ beta
  closure <- stage SClosure . pure . closureConvert $ assoc
  virtual <- stage SVirtual . genVirtual $ closure
  regalloc <- stage SRegAlloc . pure . regAlloc $ virtual
  emit regalloc
