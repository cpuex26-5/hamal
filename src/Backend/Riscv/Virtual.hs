module Backend.Riscv.Virtual (virtualify) where

import Backend.Riscv.Asm (Block (..), FunDef (..), Inst (..), Operand (..), Program (..), SingClass (..), SomeClass (..))
import Basic (Binder (..), Cmp (..))
import Basic qualified as B
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, asks, local, runReader)
import Effectful.State.Static.Local (State, gets, modify, runState)
import Middleend.Closure (Captured (..))
import Middleend.Closure qualified as C
import Name (Fresh, Id, Label (..), fresh)
import Type qualified as T

type Env = M.Map Id SomeClass

type Fnv = M.Map Float Label

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

vBlock :: (Fresh :> es, Reader Env :> es, State Fnv :> es) => SingClass c -> C.Exp -> Eff es (Block c Id Id)
vBlock c = \case
  C.Unit | SVoid <- c -> pure $ Ans Nop
  C.Int int | SGPR <- c -> pure $ Ans (Li int)
  C.Float float | SFPR <- c -> Ans . (FLwL 0) <$> (maybe new pure =<< gets @Fnv (M.lookup float))
    where
      new = do
        label <- Local <$> fresh "Lf"
        modify (M.insert float label)
          $> label
  C.Var ident | SGPR <- c -> pure . Ans . Mv . OpX $ ident
  C.Var ident | SFPR <- c -> pure . Ans . Mv . OpF $ ident
  C.PrimOp (B.Tuple binders) | SGPR <- c -> do
    tuple <- fresh "tuple"
    classes <- traverse (asks @Env . M.findWithDefault (error "unreachable")) binders
    let store (x, SomeClass c', offset) b = maybe b (\op -> Let Nothing (Store op tuple offset) b) (operand c' x)
    pure
      $ Let (Just (OpX tuple)) (Alloc (4 * length binders))
      $ foldr store (Ans (Mv (OpX tuple))) (zip3 binders classes [0, 4 ..])
  C.PrimOp op -> Ans <$> vPrimOp c op
  C.Let binder rhs body | SomeClass c' <- classOf binder.ty -> do
    rhs' <- vBlock c' rhs
    body' <- local (M.insert binder.name (SomeClass c')) (vBlock c body)
    pure $ insertLet (operand c' binder.name) rhs' body'
  C.LetTuple binders rhs body -> do
    body' <- local @Env (M.union (M.fromList [(b.name, classOf b.ty) | b <- binders])) (vBlock c body)
    pure $ foldr (withLoad rhs) body' (zip binders [0, 4 ..])
    where
      withLoad :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
      withLoad base (Binder {name, ty}, offset) k = case classOf ty of
        SomeClass SGPR -> Let (operand SGPR name) (Lw offset base) k
        SomeClass SFPR -> Let (operand SFPR name) (FLw offset base) k
        SomeClass SVoid -> k
  C.LetCls binder C.Closure {entry, actualFv} body | SomeClass c' <- classOf binder.ty -> do
    body' <- local (M.insert binder.name (SomeClass c')) (vBlock c body)
    closure <- fresh "code"
    pure
      $ Let (operand SGPR binder.name) (Alloc (4 * length actualFv + 4))
      $ Let (operand SGPR closure) (La entry)
      $ Let Nothing (Store (OpX closure) binder.name 0)
      $ foldr (withStore binder.name) body' (zip actualFv [4, 4 ..])
    where
      withStore :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
      withStore base (Binder {name, ty}, offset) k = case classOf ty of
        SomeClass SGPR -> Let Nothing (Store (OpX name) base offset) k
        SomeClass SFPR -> Let Nothing (Store (OpF name) base offset) k
        SomeClass SVoid -> k
  C.If (ICmp rel) lhs rhs thn els -> Ans <$> (If rel lhs rhs <$> vBlock c thn <*> vBlock c els)
  C.If (FCmp rel) lhs rhs thn els -> Ans <$> (FIf rel lhs rhs <$> vBlock c thn <*> vBlock c els)
  C.AppDir label args -> do
    cargs <- traverse (\n -> (n,) <$> classOfVar n) args
    let (xargs, fargs) = partitionByClass cargs
    pure $ Ans $ CallDir label xargs fargs
  C.AppCls closure args -> do
    cargs <- traverse (\n -> (n,) <$> classOfVar n) args
    let (xargs, fargs) = partitionByClass cargs
    code <- fresh "code"
    pure
      $ Let (Just (OpX code)) (Lw 0 closure)
      $ Ans (CallInd code (xargs <> [closure]) fargs)
  e -> error $ "vInst(unreachable): " <> show (c, e)
  where
    classOfVar = asks . M.findWithDefault (error "unreachable")

vPrimOp :: SingClass c -> B.PrimOp Id -> Eff es (Inst c Id Id)
vPrimOp c e = case (c, e) of
  (SGPR, B.Not ident) -> pure $ Xori ident 1
  (SGPR, B.Neg ident) -> pure $ Neg ident
  (SFPR, B.FNeg ident) -> pure $ FNeg ident
  (SGPR, B.Add lhs rhs) -> pure $ Add lhs rhs
  (SGPR, B.Sub lhs rhs) -> pure $ Sub lhs rhs
  (SGPR, B.Sali ident shamt) -> pure $ Sali ident shamt
  (SGPR, B.Sari ident shamt) -> pure $ Sari ident shamt
  (SFPR, B.FAdd lhs rhs) -> pure $ FAdd lhs rhs
  (SFPR, B.FSub lhs rhs) -> pure $ FSub lhs rhs
  (SFPR, B.FMul lhs rhs) -> pure $ FMul lhs rhs
  (SFPR, B.FDiv lhs rhs) -> pure $ FDiv lhs rhs
  (SFPR, B.FSqrt ident) -> pure $ FSqrt ident
  (SFPR, B.FloatOfInt int) -> pure $ FloatOfInt int
  (SGPR, B.IntOfFloat float) -> pure $ IntOfFloat float
  (SVoid, B.Out ident) -> pure $ Out ident
  _ -> error $ "virtualOp(unreachable): " <> show (c, e)

vFunc :: (Fresh :> es, State Fnv :> es) => C.FunDef -> Eff es (FunDef Id Id)
vFunc C.FunDef {binder, params, captured, rhs} = case binder.ty of
  T.Type (T.Fun _ ret)
    | Nothing <- captured
    , SomeClass cret <- classOf (T.Type ret) -> do
        let env = M.fromList [(b.name, classOf b.ty) | b <- params]
        rhs' <- runReader env (vBlock cret rhs)
        let (xparams, fparams) = partitionByClass [(b.name, classOf b.ty) | b <- params]
        pure $ FunDef {binder = Local binder.name, xparams, fparams, rhs = rhs', ret = cret}
  T.Type (T.Fun _ ret)
    | Just (Captured {formalFv, self}) <- captured
    , SomeClass cret <- classOf (T.Type ret) -> do
        let env = M.fromList [(b.name, classOf b.ty) | b <- params <> formalFv]
        rhs' <- runReader env (vBlock cret rhs)
        let (xparams, fparams) = partitionByClass [(b.name, classOf b.ty) | b <- params]
        let rhsWithLoad = foldr (withLoad binder.name) rhs' (zip formalFv [4, 4 ..])
        pure $ FunDef {binder = Local binder.name, xparams = xparams <> [self], fparams, rhs = rhsWithLoad, ret = cret}
    where
      withLoad :: forall c. Id -> (Binder Id T.Type, Int) -> Block c Id Id -> Block c Id Id
      withLoad base (Binder {name, ty}, offset) k = case classOf ty of
        SomeClass SGPR -> Let (operand SGPR name) (Lw offset base) k
        SomeClass SFPR -> Let (operand SFPR name) (FLw offset base) k
        SomeClass SVoid -> k
  _ -> error "unreachable"

virtualify :: (Fresh :> es) => C.Program -> Eff es (Program Id Id)
virtualify (C.Program fundefs expr) = do
  ((fundefs', main), floats) <-
    runState @Fnv M.empty
      $ runReader @Env M.empty
      $ (,) <$> traverse vFunc fundefs <*> vBlock SVoid expr
  pure $ Program (M.toList floats) fundefs' main
