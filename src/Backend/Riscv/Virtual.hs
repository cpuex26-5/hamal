module Backend.Riscv.Virtual (virtualify) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Operand (..), Program (..), SingClass (..), SomeClass (..))
import Basic (Cmp (..))
import Basic qualified as B
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Middleend.Closure qualified as C
import Name (Fresh, Id, Label (..))
import Type qualified as T

type Env = M.Map Id SomeClass

classOf :: T.Type -> SomeClass
classOf (T.Type ty) = case ty of
  T.Unit -> SomeClass SVoid
  T.Float -> SomeClass SFPR
  _ -> SomeClass SGPR

operand :: SingClass c -> Id -> Maybe (Operand c Id Id)
operand c ident = case c of
  SGPR -> Just (OpX ident)
  SFPR -> Just (OpF ident)
  SVoid -> Nothing

partitionByClass :: [(Id, SomeClass)] -> ([Id], [Id])
partitionByClass = foldr step ([], [])
  where
    step (n, SomeClass SGPR) (xs, fs) = (n : xs, fs)
    step (n, SomeClass SFPR) (xs, fs) = (xs, n : fs)
    step (_, SomeClass SVoid) (xs, fs) = (xs, fs)

insertLet :: Maybe (Operand c' x f) -> Block c' x f -> Block c x f -> Block c x f
insertLet dst (Ans inst) k = Let dst inst k
insertLet dst (Let d i rest) k = Let d i (insertLet dst rest k)

vBlock :: (Fresh :> es, Reader Env :> es) => SingClass c -> C.Exp -> Eff es (Block c Id Id)
vBlock c = \case
  C.Let binder rhs body | SomeClass c' <- classOf binder.ty -> do
    rhs' <- vBlock c' rhs
    body' <- local (M.insert binder.name (SomeClass c')) (vBlock c body)
    pure $ insertLet (operand c' binder.name) rhs' body'
  -- C.LetTuple binders rhs body -> error "unimplemented(LetTuple)"
  C.If (ICmp rel) lhs rhs thn els -> Ans <$> (If rel lhs rhs <$> vBlock c thn <*> vBlock c els)
  C.If (FCmp rel) lhs rhs thn els -> Ans <$> (FIf rel lhs rhs <$> vBlock c thn <*> vBlock c els)
  e -> Ans <$> vInst c e

vInst :: (Reader Env :> es) => SingClass c -> C.Exp -> Eff es (Inst c Id Id)
vInst c e = case (c, e) of
  (SVoid, C.Unit) -> pure Nop
  (SGPR, C.Int int) -> pure $ Li int
  (SFPR, C.Float float) -> pure $ FLi float
  (_, C.PrimOp op) -> vPrimOp c op
  (SGPR, C.Var ident) -> pure $ Mv (OpX ident)
  (SFPR, C.Var ident) -> pure $ Mv (OpF ident)
  (SVoid, C.Var _) -> pure Nop
  (_, C.AppDir label params) ->
    uncurry (CallDir label) . partitionByClass
      <$> traverse (\n -> (n,) <$> classOfVar n) params
  _ -> error "Virtual: unreachable"
  where
    classOfVar = asks . M.findWithDefault (error "unreachable")

vPrimOp :: SingClass c -> B.PrimOp Id -> Eff es (Inst c Id Id)
vPrimOp c e = case (c, e) of
  (SGPR, B.Not ident) -> pure $ Xori ident 1
  (SGPR, B.Neg ident) -> pure $ Neg ident
  (SFPR, B.FNeg ident) -> pure $ FNeg ident
  (SGPR, B.Add lhs rhs) -> pure $ Add lhs rhs
  (SGPR, B.Sub lhs rhs) -> pure $ Sub lhs rhs
  ((_, B.Mul {}); (_, B.Div {})) -> undefined
  (SFPR, B.FAdd lhs rhs) -> pure $ FAdd lhs rhs
  (SFPR, B.FSub lhs rhs) -> pure $ FSub lhs rhs
  (SFPR, B.FMul lhs rhs) -> pure $ FMul lhs rhs
  (SFPR, B.FDiv lhs rhs) -> pure $ FDiv lhs rhs
  -- B.Tuple _ -> _
  -- B.Array _ _ -> _
  -- B.Get _ _ -> _
  -- B.Put _ _ _ -> _
  (SFPR, B.FloatOfInt int) -> pure $ FloatOfInt int
  (SGPR, B.IntOfFloat float) -> pure $ IntOfFloat float
  (SVoid, B.Out ident) -> pure $ StoreL (OpX ident) (Global "UART_BASE") 0
  _ -> error "virtualOp(unreachable)"

vFunc :: (Fresh :> es) => C.FunDef -> Eff es (FunDef Id Id)
vFunc C.FunDef {binder, args, formalFv, rhs} = case binder.ty of
  T.Type (T.Fun _ ret) | SomeClass c <- classOf (T.Type ret) -> do
    let env = M.fromList [(b.name, classOf b.ty) | b <- args <> formalFv]
    rhs' <- runReader env (vBlock c rhs)
    let (_, load) =
          foldr
            ( \fv (offset, rest) -> case classOf fv.ty of
                SomeClass SGPR -> (offset + 4, Let (operand SGPR fv.name) (LwL offset binder.name) rest)
                SomeClass SFPR -> (offset + 4, Let (operand SFPR fv.name) (FLwL offset binder.name) rest)
                SomeClass SVoid -> (offset, rest)
            )
            (4, rhs')
            formalFv
    let (xargs, fargs) = partitionByClass [(b.name, classOf b.ty) | b <- args]
    pure $ FunDef {binder = binder.name, xargs, fargs, rhs = load, ret = c}
  _ -> error "unreachable"

virtualify :: (Fresh :> es) => C.Program -> Eff es (Program Id Id)
virtualify (C.Program fundefs expr) =
  runReader @Env M.empty $
    Program <$> traverse vFunc fundefs <*> vBlock SVoid expr
