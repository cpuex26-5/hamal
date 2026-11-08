module Hamal (Inspect (..), Phase (..), Stage (..), phase, compile, CompileError (..), phaseName) where

import Backend.Riscv.Asm (FReg, XReg)
import Backend.Riscv.Asm qualified as V
import Backend.Riscv.Emit (emit)
import Backend.Riscv.RegAlloc (regAlloc)
import Backend.Riscv.Virtual (virtualify)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error, runErrorNoCallStackWith, throwError)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)
import Frontend.Syntax (Parsed, Typed)
import Frontend.Typing (TypeError, typing)
import Log (Log)
import Middleend.Closure (closureConvert)
import Middleend.Closure qualified as C
import Middleend.KNormal (kNormal)
import Middleend.KNormal qualified as K
import Middleend.Opt.Assoc (assocLet)
import Middleend.Opt.Beta (betaConvert)
import Name (Id, runFresh)

data Phase
  = Parsed
  | Typed
  | KNormal
  | Beta
  | Assoc
  | Closure
  | Virtual
  | RegAlloc
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Stage a where
  SParsed :: Stage Parsed
  STyped :: Stage Typed
  SKNormal :: Stage K.Exp
  SBeta :: Stage K.Exp
  SAssoc :: Stage K.Exp
  SClosure :: Stage C.Program
  SVirtual :: Stage (V.Program Id Id)
  SRegAlloc :: Stage (V.Program XReg FReg)

phase :: Stage a -> Phase
phase = \case
  SParsed -> Parsed
  STyped -> Typed
  SKNormal -> KNormal
  SBeta -> Beta
  SAssoc -> Assoc
  SClosure -> Closure
  SVirtual -> Virtual
  SRegAlloc -> RegAlloc

phaseName :: Phase -> String
phaseName = \case
  Parsed -> "parsed"
  Typed -> "typed"
  KNormal -> "knorm"
  Beta -> "beta"
  Assoc -> "assoc"
  Closure -> "closure"
  Virtual -> "virtual"
  RegAlloc -> "regalloc"

data Inspect :: Effect where
  Inspect :: (Show a) => Stage a -> a -> Inspect m ()

type instance DispatchOf Inspect = Dynamic

data CompileError = ParseError String | TypeError TypeError
  deriving stock (Show, Eq)

stage :: (Show a, Inspect :> es) => Stage a -> Eff es a -> Eff es a
stage s m = do
  x <- m
  send (Inspect s x)
  pure x

compile :: (Error CompileError :> es, Inspect :> es, Log :> es) => ByteString -> Eff es Builder
compile input = runFresh do
  syntax <- stage SParsed . either (throwError . ParseError) pure $ runAlex input parse
  typed <- stage STyped . runErrorNoCallStackWith (throwError . TypeError) $ typing syntax
  knorm <- stage SKNormal . kNormal $ typed
  beta <- stage SBeta . betaConvert $ knorm
  assoc <- stage SAssoc . pure . assocLet $ beta
  closure <- stage SClosure . pure . closureConvert $ assoc
  virtual <- stage SVirtual . virtualify $ closure
  regalloc <- stage SRegAlloc . pure . regAlloc $ virtual
  emit regalloc
